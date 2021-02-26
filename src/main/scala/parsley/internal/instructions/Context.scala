package parsley.internal.instructions

import Stack.{drop, isEmpty, mkString, map, push}
import parsley.{Failure, Result, Success}
import parsley.internal.errors.{
    TrivialError,
    ErrorItem, Desc,
    LineBuilder, ErrorItemBuilder,
    DefuncError, ClassicExpectedError, ClassicExpectedErrorWithReason, ClassicFancyError, ClassicUnexpectedError, WithHints,
    DefuncHints, EmptyHints, MergeHints, ReplaceHint, PopHints, AddError
}
import Context.{Frame, State, Handler, Hints}

import scala.annotation.tailrec
import scala.collection.mutable

private [parsley] object Context {
    private [Context] val NumRegs = 4
    private [parsley] def empty: Context = new Context(null, "")

    // Private internals
    private [Context] final class Frame(val ret: Int, val instrs: Array[Instr]) {
        override def toString: String = s"[$instrs@$ret]"
    }
    private [Context] final class Handler(val depth: Int, val pc: Int, var stacksz: Int) {
        override def toString: String = s"Handler@$depth:$pc(-${stacksz + 1})"
    }
    private [Context] final class State(val offset: Int, val line: Int, val col: Int) {
        override def toString: String = s"$offset ($line, $col)"
    }
    private [Context] final class Hints(val hints: DefuncHints, val validOffset: Int) {
        override def toString: String = s"($validOffset, $hints)"
    }
}

private [parsley] final class Context(private [instructions] var instrs: Array[Instr],
                                      private [instructions] var input: String,
                                      private val sourceName: Option[String] = None) {
    /** This is the operand stack, where results go to live  */
    private [instructions] val stack: ArrayStack[Any] = new ArrayStack()
    /** Current offset into the input */
    private [instructions] var offset: Int = 0
    /** The length of the input, stored for whatever reason */
    private [instructions] var inputsz: Int = input.length
    /** Call stack consisting of Frames that track the return position and the old instructions */
    private var calls: Stack[Frame] = Stack.empty
    /** State stack consisting of offsets and positions that can be rolled back */
    private [instructions] var states: Stack[State] = Stack.empty
    /** Stack consisting of offsets at previous checkpoints, which may query to test for consumed input */
    private [instructions] var checkStack: Stack[Int] = Stack.empty
    /** Current operational status of the machine */
    private [instructions] var status: Status = Good
    /** Stack of handlers, which track the call depth, program counter and stack size of error handlers */
    private [instructions] var handlers: Stack[Handler] = Stack.empty
    /** Current size of the call stack */
    private var depth: Int = 0
    /** Current offset into program instruction buffer */
    private [instructions] var pc: Int = 0
    /** Current line number */
    private [instructions] var line: Int = 1
    /** Current column number */
    private [instructions] var col: Int = 1
    /** State held by the registers, AnyRef to allow for `null` */
    private [instructions] val regs: Array[AnyRef] = new Array[AnyRef](Context.NumRegs)
    /** Amount of indentation to apply to debug combinators output */
    private [instructions] var debuglvl: Int = 0

    // NEW ERROR MECHANISMS
    private var hints: DefuncHints = EmptyHints
    private var hintsValidOffset = 0
    private var hintStack = Stack.empty[Hints]
    private [instructions] var errs: Stack[DefuncError] = Stack.empty

    private [instructions] def saveHints(shadow: Boolean): Unit = {
        hintStack = push(hintStack, new Hints(hints, hintsValidOffset))
        if (!shadow) hints = EmptyHints
    }
    private [instructions] def restoreHints(): Unit = {
        val hintFrame = this.hintStack.head
        this.hintsValidOffset = hintFrame.validOffset
        this.hints = hintFrame.hints
        this.commitHints()
    }
    private [instructions] def commitHints(): Unit = {
        this.hintStack = this.hintStack.tail
    }

    /* ERROR RELABELLING BEGIN */
    private [instructions] def mergeHints(): Unit = {
        val hintFrame = this.hintStack.head
        if (hintFrame.validOffset == offset) this.hints = MergeHints(hintFrame.hints, this.hints)
        commitHints()
    }
    private [instructions] def replaceHint(label: String): Unit = hints = ReplaceHint(label, hints)
    private [instructions] def popHints: Unit = hints = PopHints(hints)
    /* ERROR RELABELLING END */

    private def addErrorToHints(): Unit = {
        val err = errs.head
        if (err.isTrivialError && err.offset == offset && !err.isExpectedEmpty) {
            // If our new hints have taken place further in the input stream, then they must invalidate the old ones
            if (hintsValidOffset < offset) {
                hints = EmptyHints
                hintsValidOffset = offset
            }
            hints = new AddError(hints, err)
        }
    }
    private [instructions] def addErrorToHintsAndPop(): Unit = {
        this.addErrorToHints()
        this.errs = this.errs.tail
    }

    private [instructions] def updateCheckOffsetAndHints() = {
        this.checkStack.head = this.offset
        this.hintsValidOffset = this.offset
    }

    // $COVERAGE-OFF$
    private [instructions] def pretty: String = {
        s"""[
           |  stack     = [${stack.mkString(", ")}]
           |  instrs    = ${instrs.mkString("; ")}
           |  input     = ${input.drop(offset).mkString}
           |  pos       = ($line, $col)
           |  status    = $status
           |  pc        = $pc
           |  depth     = $depth
           |  rets      = ${mkString(map[Frame, Int](calls, _.ret), ", ")}
           |  handlers  = ${mkString(handlers, ":")}[]
           |  recstates = ${mkString(states, ":")}[]
           |  checks    = ${mkString(checkStack, ":")}[]
           |  registers = ${regs.zipWithIndex.map{case (r, i) => s"r$i = $r"}.mkString("\n              ")}
           |  errors    = ${mkString(errs, ":")}[]
           |  hints     = ($hintsValidOffset, ${hints.toList}):${mkString(hintStack, ":")}[]
           |]""".stripMargin
    }
    // $COVERAGE-ON$

    @tailrec @inline private [parsley] def runParser[A](): Result[A] = {
        //println(pretty)
        if (status eq Failed) Failure(errs.head.asParseError.pretty(sourceName))
        else if (pc < instrs.length) {
            instrs(pc)(this)
            runParser[A]()
        }
        else if (isEmpty(calls)) Success(stack.peek[A])
        else {
            ret()
            runParser[A]()
        }
    }

    private [instructions] def call(newInstrs: Array[Instr], at: Int) = {
        calls = push(calls, new Frame(pc + 1, instrs))
        instrs = newInstrs
        pc = at
        depth += 1
    }

    private [instructions] def ret(): Unit = {
        val frame = calls.head
        instrs = frame.instrs
        calls = calls.tail
        pc = frame.ret
        depth -= 1
    }

    private [instructions] def catchNoConsumed(handler: =>Unit): Unit = {
        if (offset != checkStack.head) fail()
        else {
            status = Good
            handler
        }
        checkStack = checkStack.tail
    }

    private [instructions] def pushError(err: DefuncError): Unit = this.errs = push(this.errs, this.useHints(err))
    private [instructions] def useHints(err: DefuncError): DefuncError = {
        if (hintsValidOffset == offset) WithHints(err, hints)
        else {
            hintsValidOffset = offset
            hints = EmptyHints
            err
        }
    }

    private [instructions] def failWithMessage(msg: String): Unit = {
        this.fail(new ClassicFancyError(offset, line, col, msg))
    }
    private [instructions] def unexpectedFail(expected: Option[ErrorItem], unexpected: ErrorItem): Unit = {
        this.fail(new ClassicUnexpectedError(offset, line, col, expected, unexpected))
    }
    private [instructions] def expectedFail(expected: Option[ErrorItem]): Unit = {
        this.fail(new ClassicExpectedError(offset, line, col, expected))
    }
    private [instructions] def expectedFail(expected: Option[ErrorItem], reason: String): Unit = {
        this.fail(new ClassicExpectedErrorWithReason(offset, line, col, expected, reason))
    }
    private [instructions] def fail(error: DefuncError): Unit = {
        this.pushError(error)
        this.fail()
    }
    private [instructions] def fail(): Unit = {
        if (isEmpty(handlers)) status = Failed
        else {
            status = Recover
            val handler = handlers.head
            handlers = handlers.tail
            val diffdepth = depth - handler.depth - 1
            if (diffdepth >= 0) {
                val calls_ = drop(calls, diffdepth)
                instrs = calls_.head.instrs
                calls = calls_.tail
            }
            pc = handler.pc
            val diffstack = stack.usize - handler.stacksz
            if (diffstack > 0) stack.drop(diffstack)
            depth = handler.depth
        }
    }

    private [instructions] def pushAndContinue(x: Any) = {
        stack.push(x)
        inc()
    }
    private [instructions] def exchangeAndContinue(x: Any) = {
        stack.exchange(x)
        inc()
    }
    private [instructions] def inc(): Unit = pc += 1
    private [instructions] def nextChar: Char = input.charAt(offset)
    private [instructions] def moreInput: Boolean = offset < inputsz
    private [instructions] def updatePos(c: Char) = c match {
        case '\n' => line += 1; col = 1
        case '\t' => col += 4 - ((col - 1) & 3)
        case _ => col += 1
    }
    private [instructions] def consumeChar(): Char = {
        val c = nextChar
        updatePos(c)
        offset += 1
        c
    }
    private [instructions] def fastUncheckedConsumeChars(n: Int) = {
        offset += n
        col += n
    }
    private [instructions] def pushHandler(label: Int): Unit = {
        handlers = push(handlers, new Handler(depth, label, stack.usize))
    }
    private [instructions] def pushCheck(): Unit = checkStack = push(checkStack, offset)
    private [instructions] def saveState(): Unit = states = push(states, new State(offset, line, col))
    private [instructions] def restoreState(): Unit = {
        val state = states.head
        states = states.tail
        offset = state.offset
        line = state.line
        col = state.col
    }
    private [instructions] def writeReg(reg: Int, x: Any): Unit = {
        regs(reg) = x.asInstanceOf[AnyRef]
    }

    // Allows us to reuse a context, helpful for benchmarking and potentially user applications
    private [parsley] def apply(_instrs: Array[Instr], _input: String): Context = {
        instrs = _instrs
        input = _input
        stack.clear()
        offset = 0
        inputsz = input.length
        calls = Stack.empty
        states = Stack.empty
        checkStack = Stack.empty
        status = Good
        handlers = Stack.empty
        depth = 0
        pc = 0
        line = 1
        col = 1
        debuglvl = 0
        hintsValidOffset = 0
        hints = EmptyHints
        hintStack = Stack.empty
        this
    }

    private implicit val lineBuilder: LineBuilder = new LineBuilder {
        def nearestNewlineBefore(off: Int): Int = {
            val idx = Context.this.input.lastIndexOf('\n', off-1)
            if (idx == -1) 0 else idx + 1
        }
        def nearestNewlineAfter(off: Int): Int = {
            val idx = Context.this.input.indexOf('\n', off)
            if (idx == -1) Context.this.inputsz else idx
        }
        def segmentBetween(start: Int, end: Int): String = {
            Context.this.input.substring(start, end)
        }
    }

    private implicit val errorItemBuilder: ErrorItemBuilder = new ErrorItemBuilder {
        def inRange(offset: Int): Boolean = offset < Context.this.inputsz
        def charAt(offset: Int): Char = Context.this.input.charAt(offset)
        def substring(offset: Int, size: Int): String = Context.this.input.substring(offset, Math.min(offset + size, Context.this.inputsz))
    }
}