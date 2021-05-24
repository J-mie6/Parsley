package parsley

import parsley.Parsley.{empty, pure}
import parsley.combinator.{when, whileP}
import parsley.internal.deepembedding

/** This module contains all the functionality and operations for using and manipulating registers.
  * @since 2.2.0
  */
object registers {
    /**
    * This class is used to index registers within the mutable state.
    * Currently, there are only 4 available registers, so use them wisely!
    *
    * If you need more than four registers but know that they will be used
    * at different times you can rename your register, as long as they point
    * to the same reference. You may find the
    * [[parsley.Parsley$.LazyParsley.cast[B]* Parsley[A].cast[B: ClassTag]: Parsley[B] ]]
    * combinator useful to change the type of a `Reg[Any]`.
    *
    * @note It is undefined behaviour to use a register in multiple different
    *       independent parsers. You should be careful to parameterise the
    *       registers in shared parsers and allocate fresh ones for each "top-level"
    *       parser you will run.
    * @since 2.2.0
    */
    class Reg[A] private [parsley] {
        /**
          * Consumes no input and returns the value stored in this register
          * @return The value stored in register
          * @since 3.2.0
          */
        def get: Parsley[A] = new Parsley(new deepembedding.Get(this))
        /**
          * Consumes no input and returns the value stored in this register after applying a function.
          * @param f The function used to transform the value in this register
          * @tparam B The desired result type
          * @return The value stored in this register applied to `f`
          * @since 3.2.0
          */
        def gets[B](f: A => B): Parsley[B] = this.gets(pure(f))
        /**
          * Returns the value stored this register after applying a function obtained from given parser.
          * @note The value is fetched after `pf` is executed
          * @param pf The parser which provides the function to transform values
          * @tparam B The desired result type
          * @return The value stored in this register applied to `f` from `pf`
          * @since 3.2.0
          */
        def gets[B](pf: Parsley[A => B]): Parsley[B] = pf <*> this.get
        /**
          * Consumes no input and places the value `x` into this register.
          * @param x The value to place in the register
          * @since 3.2.0
          */
        def put(x: A): Parsley[Unit] = this.put(pure(x))
        /**
          * Places the result of running `p` into this register.
          * @param p The parser to derive the value from
          * @since 3.2.0
          */
        def put(p: =>Parsley[A]): Parsley[Unit] = new Parsley(new deepembedding.Put(this, p.internal))
        /**
          * Places the result of running `p` into this register.
          * @param p The parser to derive the value from
          * @param f A function which adapts the result of `p` so that it can fit in `r`
          * @since 3.0.0
          */
        def puts[B](p: =>Parsley[B], f: B => A): Parsley[Unit] = this.put(p.map(f))
        /**
          * Modifies the value contained in this register using function `f`.
          * @param f The function used to modify the register
          * @since 3.2.0
          */
        def modify(f: A => A): Parsley[Unit] = new Parsley(new deepembedding.Modify(this, f))
        /**
          * Modifies the value contained in this register using function `f` obtained from executing `p`.
          * @note The value is modified after `pf` is executed
          * @param f The function used to modify the register
          * @since 3.2.0
          */
        def modify(pf: =>Parsley[A => A]): Parsley[Unit] = this.put(this.gets(pf))
        /**
          * For the duration of parser `p` the state stored in this register is instead set to `x`. The change is undone
          * after `p` has finished.
          * @param x The value to place into this register
          * @param p The parser to execute with the adjusted state
          * @return The parser that performs `p` with the modified state
          * @since 3.2.0
          */
        def local[B](x: A)(p: =>Parsley[B]): Parsley[B] = this.local(pure(x))(p)
        /**
          * For the duration of parser `q` the state stored in this register is instead set to the return value of `p`. The
          * change is undone after `q` has finished.
          * @param p The parser whose return value is placed in this register
          * @param q The parser to execute with the adjusted state
          * @return The parser that performs `q` with the modified state
          * @since 3.2.0
          */
        def local[B](p: =>Parsley[A])(q: =>Parsley[B]): Parsley[B] = new Parsley(new deepembedding.Local(this, p.internal, q.internal))
        /**
          * For the duration of parser `p` the state stored in this register is instead modified with `f`. The change is undone
          * after `p` has finished.
          * @param f The function used to modify the value in this register
          * @param p The parser to execute with the adjusted state
          * @return The parser that performs `p` with the modified state
          * @since 3.2.0
          */
        def local[B](f: A => A)(p: =>Parsley[B]): Parsley[B] = this.local(this.gets(f))(p)

        /** `rollback(reg, p)` will perform `p`, but if it fails without consuming input, any changes to this register will
          * be reverted.
          * @param p The parser to perform
          * @return The result of the parser `p`, if any
          * @since 3.2.0
          */
        def rollback[B](p: Parsley[B]): Parsley[B] = {
            this.get.persist(x => {
                p <|> (this.put(x) *> empty)
            })
        }

        private [parsley] var _v: Int = -1
        private [parsley] def addr: Int = {
            assert(allocated)
            _v
        }
        private [parsley] def allocated: Boolean = _v != -1
        private [parsley] def allocate(v: Int): Unit = {
            assert(!allocated)
            this._v = v
        }
        //override def toString: String = s"Reg(${if (allocated) addr else "unallocated"})"
    }
    object Reg {
        /**
        * @tparam A The type to be contained in this register during runtime
        * @return A new register which can contain the given type
        * @since 2.2.0
        */
        def make[A]: Reg[A] = new Reg
    }

    // $COVERAGE-OFF$
    /**
      * Consumes no input and returns the value stored in one of the parser registers.
      * @param r The index of the register to collect from
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @return The value stored in register `r` of type `S`
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.get instead", "3.2.0")
    def get[S](r: Reg[S]): Parsley[S] = r.get
    /**
      * Consumes no input and returns the value stored in one of the parser registers after applying a function.
      * @param r The index of the register to collect from
      * @param f The function used to transform the value in the register
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f`
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.gets(f) instead", "3.2.0")
    def gets[S, A](r: Reg[S], f: S => A): Parsley[A] = r.gets(f)
    /**
      * Returns the value stored in one of the parser registers after applying a function obtained from given parser.
      * @note The value is fetched after `pf` is executed
      * @param r The index of the register to collect from
      * @param pf The parser which provides the function to transform values
      * @tparam S The type of the value in register `r` (this will result in a runtime type-check)
      * @tparam A The desired result type
      * @return The value stored in register `r` applied to `f` from `pf`
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.gets(pf) instead", "3.2.0")
    def gets[S, A](r: Reg[S], pf: Parsley[S => A]): Parsley[A] = r.gets(pf)
    /**
      * Consumes no input and places the value `x` into register `r`.
      * @param r The index of the register to place the value in
      * @param x The value to place in the register
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.put(x) instead", "3.2.0")
    def put[S](r: Reg[S], x: S): Parsley[Unit] = r.put(x)
    /**
      * Places the result of running `p` into register `r`.
      * @param r The index of the register to place the value in
      * @param p The parser to derive the value from
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.put(p) instead", "3.2.0")
    def put[S](r: Reg[S], p: =>Parsley[S]): Parsley[Unit] = r.put(p)
    /**
      * Places the result of running `p` into register `r`.
      * @param r The index of the register to place the value in
      * @param p The parser to derive the value from
      * @param f A function which adapts the result of `p` so that it can fit in `r`
      * @since 3.0.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.puts(p, f) instead", "3.2.0")
    def puts[A, S](r: Reg[S], p: =>Parsley[A], f: A => S): Parsley[Unit] = r.puts(p, f)
    /**
      * Modifies the value contained in register `r` using function `f`.
      * @param r The index of the register to modify
      * @param f The function used to modify the register
      * @tparam S The type of value currently assumed to be in the register
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.modify(f) instead", "3.2.0")
    def modify[S](r: Reg[S], f: S => S): Parsley[Unit] = r.modify(f)
    /**
      * For the duration of parser `p` the state stored in register `r` is instead set to `x`. The change is undone
      * after `p` has finished.
      * @param r The index of the register to modify
      * @param x The value to place in the register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.local(x)(p) instead", "3.2.0")
    def local[R, A](r: Reg[R], x: R, p: =>Parsley[A]): Parsley[A] = r.local(x)(p)
    /**
      * For the duration of parser `q` the state stored in register `r` is instead set to the return value of `p`. The
      * change is undone after `q` has finished.
      * @param r The index of the register to modify
      * @param p The parser whose return value is placed in register `r`
      * @param q The parser to execute with the adjusted state
      * @return The parser that performs `q` with the modified state
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.local(p)(q) instead", "3.2.0")
    def local[R, A](r: Reg[R], p: =>Parsley[R], q: =>Parsley[A]): Parsley[A] = r.local(p)(q)
    /**
      * For the duration of parser `p` the state stored in register `r` is instead modified with `f`. The change is undone
      * after `p` has finished.
      * @note There are only 4 registers at present.
      * @param r The index of the register to modify
      * @param f The function used to modify the value in register `r`
      * @param p The parser to execute with the adjusted state
      * @return The parser that performs `p` with the modified state
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use r.local(f)(p) instead", "3.2.0")
    def local[R, A](r: Reg[R], f: R => R, p: =>Parsley[A]): Parsley[A] = r.local(f)(p)

    /** `rollback(reg, p)` will perform `p`, but if it fails without consuming input, any changes to the register `reg` will
      * be reverted.
      * @param p The parser to perform
      * @param reg The register to rollback on failure of `p`
      * @return The result of the parser `p`, if any
      * @since 2.2.0
      */
    @deprecated("This combinator will be removed in 4.0.0, use reg.rollback(p) instead", "3.2.0")
    def rollback[A, B](reg: Reg[A], p: Parsley[B]): Parsley[B] = reg.rollback(p)
    // $COVERAGE-ON$

    implicit final class RegisterMethods[P, A](p: =>P)(implicit con: P => Parsley[A]) {
        def fillReg[B](body: Reg[A] => Parsley[B]): Parsley[B] = {
            val reg = Reg.make[A]
            reg.put(con(p)) *> body(reg)
        }
        def persist[B](f: Parsley[A] => Parsley[B]): Parsley[B] = con(p).flatMap(x => f(pure(x)))//this.fillReg(reg => f(get(reg)))
    }

    implicit final class RegisterMaker[A](x: A) {
        def makeReg[B](body: Reg[A] => Parsley[B]): Parsley[B] = pure(x).fillReg(body)
    }

    /** `forP(v, init, cond, step, body)` behaves much like a traditional for loop using variable `v` as the loop
      * variable and `init`, `cond`, `step` and `body` as parsers which control the loop itself. This is useful for
      * performing certain context sensitive tasks. For instance, to read an equal number of as, bs and cs you can do:
      *
      * {{{
      * val r = Reg.make[Int]
      * put(r, 0) *>
      * many('a' *> modify[Int](r, _+1)) *>
      * forP[Int](get](r), pure(_ != 0), pure(_ - 1), 'b') *>
      * forP[Int](get(r), pure(_ != 0), pure(_ - 1), 'c')
      * }}}
      *
      * @param init The initial value of the induction variable
      * @param cond The condition by which the loop terminates
      * @param step The change in induction variable on each iteration
      * @param body The body of the loop performed each iteration
      * @return ()
      */
    def forP[A](init: =>Parsley[A], cond: =>Parsley[A => Boolean], step: =>Parsley[A => A], body: =>Parsley[_]): Parsley[Unit] =
    {
        val reg = Reg.make[A]
        val _cond = reg.gets(cond)
        val _step = reg.modify(step)
        reg.put(init) *> when(_cond, whileP(body *> _step *> _cond))
    }
}