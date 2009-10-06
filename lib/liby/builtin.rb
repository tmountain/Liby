require 'liby/lobject'
require 'liby/objectmanager'

class Builtin
  def initialize(o_man, lint)
    @o_man      = o_man
    @lint       = lint
    @intrinsics = {"%"               => "func_mod",
                   "&dump-memory"    => "func_dump_memory",
                   "*"               => "func_mul",
                   "+"               => "func_add",
                   "-"               => "func_sub",
                   "/"               => "func_div",
                   "<"               => "func_lt",
                   "<="              => "func_le",
                   "="               => "func_numeq",
                   ">"               => "func_gt",
                   ">="              => "func_ge",
                   "and"             => "func_and",
                   "atom"            => "func_atom",
                   "car"             => "func_car",
                   "cdr"             => "func_cdr",
                   "cond"            => "func_cond",
                   "cons"            => "func_cons",
                   "defun"           => "func_defun",
                   "eq"              => "func_eq",
                   "eval"            => "func_eval",
                   "garbage-collect" => "func_gc",
                   "gc"              => "func_gc",
                   "if"              => "func_if",
                   "list"            => "func_list",
                   "not"             => "func_not",
                   "null"            => "func_not",
                   "or"              => "func_or",
                   "princ"           => "func_princ",
                   "prog1"           => "func_prog1",
                   "prog2"           => "func_prog2",
                   "progn"           => "func_progn",
                   "quote"           => "func_quote",
                   "set"             => "func_set",
                   "setf"            => "func_setq",
                   "setq"            => "func_setq",
                   "unless"          => "func_unless",
                   "when"            => "func_when",
                   "while"           => "func_while"}
  end

  def count_list(p)
    i = 0

    while p != @o_man.nil and p.type == LObject::OBJ_CONS
      p = p.value.c.cdr
      i += 1
    end

    return i
  end

  def car(p)
    if p.type == LObject::OBJ_CONS
      return p.value.c.car
    end

    if @lint and p != @o_man.nil and p != @o_man.t
      Error.warn("warning: getting the CAR of a non cons object")
    end
    return @o_man.nil
  end

  # Syntax: (car expr)
  def func_car(args)
    return car(_eval(car(args)))
  end

  def cdr(p)
    if p.type == LObject::OBJ_CONS
      return p.value.c.cdr
    end

    if @lint and p != @o_man.nil and p != @o_man.t
      Error.warn("warning: getting the CDR of a non cons object")
    end
    return @o_man.nil
  end

  # Syntax: (cdr expr)
  def func_cdr(args)
    return cdr(_eval(car(args)))
  end

  # Syntax: (+ expr...)
  def func_add(args)
    p = args
    v = 0

    loop do
      p1 = _eval(car(p))
      if p1.type == LObject::OBJ_INTEGER
        v += p1.value.i
      elsif (@lint)
        Error.warn("warning: sum with a non integer operand")
      end

      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    p1 = @o_man.new_object(LObject::OBJ_INTEGER)
    p1.value.i = v
    return p1
  end

  # Syntax: (- expr...)
  def func_sub(args)
    p = args
    v = 0

    loop do
      p1 = _eval(car(p))
      if p1.type == LObject::OBJ_INTEGER
        if p == args and cdr(p) != @o_man.nil
          v = p1.value.i
        else
          v -= p1.value.i
        end
        p = cdr(p)
      elsif @lint
        Error.warn("warning: difference with a non integer operand")
      end
      break unless (p != @o_man.nil)
    end

    p1 = @o_man.new_object(LObject::OBJ_INTEGER)
    p1.value.i = v
    return p1
  end

  # Syntax: (* expr...)
  def func_mul(args)
    p = args
    v = 1
    
    loop do
      p1 = _eval(car(p))

      if (p1.type == LObject::OBJ_INTEGER)
        v *= p1.value.i
      elsif @lint
        Error.warn("warning: product with a non integer operand")
      end
      p = cdr(p)

      break unless (p != @o_man.nil)
    end

    p1 = @o_man.new_object(LObject::OBJ_INTEGER)
    p1.value.i = v
    return p1
  end

  # Syntax: (/ expr...)
  def func_div(args)
    p = args
    v = 0

    loop do
      p1 = _eval(car(p))
      if p1.type == LObject::OBJ_INTEGER
        if p == args and cdr(p) != @o_man.nil
          v = p1.value.i
        else
          if p1.value.i == 0
            if @lint
              Error.warn("warning: division by zero")
            end
            v = 0
            break
          else
            v /= p1.value.i
          end
        end
      elsif @lint
        Error.warn("warning: quotient with a non integer operand");
      end

      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    p1 = @o_man.new_object(LObject::OBJ_INTEGER)
    p1.value.i = v
    return p1
  end

  # Syntax: (% expr1 expr2)
  def func_mod(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER or p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: modulo with a non integer operand")
      end
      return @o_man.nil
    end

    p3 = @o_man.new_object(LObject::OBJ_INTEGER)
    if p2.value.i == 0
      if @lint
        Error.warn("warning: modulo by zero")
      end
      p3.value.i = 0
    else
      p3.value.i = p1.value.i % p2.value.i
    end

    return p3
  end


  # Syntax: (< expr1 expr2)
  def func_lt(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER or p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: comparison with a non integer operand")
      end
      return @o_man.nil
    end

    if (p1.value.i < p2.value.i)
      return @o_man.t
    end

    return @o_man.nil
  end

  # Syntax: (> expr1 expr2)
  def func_gt(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER or p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: comparison with a non integer operand")
      end
      return @o_man.nil
    end

    if (p1.value.i > p2.value.i)
      return @o_man.t
    end

    return @o_man.nil
  end

  # Syntax: (<= expr1 expr2)
  def func_le(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER and p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: comparison with a non integer operand")
      end
      return @o_man.nil
    end

    if p1.value.i <= p2.value.i
      return @o_man.t
    end
    return @o_man.nil
  end

  # Syntax: (>= expr1 expr2)
  def func_ge(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER and p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: comparison with a non integer operand")
      end
      return @o_man.nil
    end

    if p1.value.i >= p2.value.i
      return @o_man.t
    end
    return @o_man.nil
  end

  # Syntax: (= expr1 expr2)
  def func_numeq(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1.type != LObject::OBJ_INTEGER or p2.type != LObject::OBJ_INTEGER
      if @lint
        Error.warn("warning: comparison with a non integer operand")
      end
      return @o_man.nil
    end

    if p1.value.i == p2.value.i
      return @o_man.t
    end
    return @o_man.nil
  end

  def princ_string(fout, s)
    fout.print('"')
    s.each_byte do |c|
      c = c.chr
      case c
        when "\a"
          fout.print("\\a")
        when "\b"
          fout.print("\\b")
        when "\f"
          fout.print("\\f")
        when "\n"
          fout.print("\\n")
        when "\r"
          fout.print("\\r")
        when "\t"
          fout.print("\\t")
        when "\v"
          fout.print("\\v")
        else
          fout.print(c)
      end
    end
    fout.print('"')
  end

  def princ_object(fout, p)
    case p.type
      when LObject::OBJ_NIL
        fout.print("nil")
      when LObject::OBJ_T
        fout.print("t")
      when LObject::OBJ_IDENTIFIER
        fout.print(p.value.id)
      when LObject::OBJ_STRING
        princ_string(fout, p.value.s)
      when LObject::OBJ_INTEGER
        fout.print(p.value.i)
      when LObject::OBJ_CONS
        fout.print("(")
        p1 = p
        loop do
          princ_object(fout, p1.value.c.car)
          p1 = p1.value.c.cdr
          if p1 != @o_man.nil
            fout.print(" ")
            if p1.type != LObject::OBJ_CONS
              fout.print(". ")
              princ_object(fout, p1)
            end
          end
          break unless (p1 != @o_man.nil and p1.type == LObject::OBJ_CONS)
        end
        fout.print(")")
    end
  end

  # Syntax: (princ expr...)
  def func_princ(args)
    p = args
    p1 = nil

    loop do
      p1 = _eval(car(p))
      if p1.type == LObject::OBJ_STRING
        print(p1.value.s)
      else
        princ_object($stdout, p1)
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    return p1
  end

  # Syntax: (atom expr)
  def func_atom(args)
    p = _eval(car(args))

    if p.type == LObject::OBJ_T or
       p.type == LObject::OBJ_NIL or
       p.type == LObject::OBJ_INTEGER or
       p.type == LObject::OBJ_STRING or
       p.type == LObject::OBJ_IDENTIFIER
      return @o_man.t
    end

    return @o_man.nil
  end

  # Syntax: (cons expr1 expr2)
  def func_cons(args)
    p = @o_man.new_object(LObject::OBJ_CONS)
    p.value.c.car = _eval(car(args))
    p.value.c.cdr = _eval(car(cdr(args)))
    return p
  end

  # Syntax: (list expr1...)
  def func_list(args)
    p     = args
    first = nil
    prev  = nil

    if p == @o_man.nil
      return @o_man.nil
    end

    loop do
      p1 = @o_man.new_object(LObject::OBJ_CONS)
      p1.value.c.car = _eval(car(p))

      if !first
        first = p1
      end

      if prev
        prev.value.c.cdr = p1
      end

      prev = p1
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    return first
  end

  # Syntax: (eq expr1 expr2)
  def func_eq(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1 == p2
      return @o_man.t
    end

    if p1.type == LObject::OBJ_CONS or p2.type == LObject::OBJ_CONS
      return @o_man.nil
    end

    if p1.type == p2.type
      case p1.type
        when LObject::OBJ_IDENTIFIER
          if p1.value.id == p2.value.id
            return @o_man.t
          end
          return @o_man.nil
        when LObject::OBJ_STRING:
          if p1.value.s == p2.value.s
            return @o_man.t
          end
          return @o_man.nil
        when LObject::OBJ_INTEGER
          if p1.value.i == p2.value.i
            return @o_man.t
          end
          return @o_man.nil
      end
    end
    return @o_man.nil
  end

  # Syntax: (quote expr)
  def func_quote(args)
    return car(args)
  end

  # Syntax: (and expr...)
  def func_and(args)
    p = args
    p1 = nil

    loop do
      p1 = _eval(car(p))
      if p1 == @o_man.nil
        return @o_man.nil
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    return p1
  end

  # Syntax: (or expr...)
  def func_or(args)
    p = args

    loop do
      p1 = _eval(car(p))
      if p1 != @o_man.nil
        return p1
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end
    return @o_man.nil
  end

  # Syntax: (not expr)
  # Syntax: (null expr)
  def func_not(args)
    p = _eval(car(args))

    if p != @o_man.nil
      return @o_man.nil
    end

    return @o_man.t
  end

  # Syntax: (cond (expr1 [expr2])...)
  def func_cond(args)
    p = args

    loop do
      p1 = car(p)
      if (p2 = _eval(car(p1))) != @o_man.nil
        if (p3 = cdr(p1)) != @o_man.nil
          return func_progn(p3)
        end
        return p2
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end
    return @o_man.nil
  end

  # Syntax: (if expr then-expr else-expr...)
  def func_if(args)
    p1 = car(args)
    p2 = car(cdr(args))
    p3 = cdr(cdr(args))

    if _eval(p1) != @o_man.nil
      return _eval(p2)
    end

    return func_progn(p3)
  end

  # Syntax: (when expr then-expr...)
  def func_when(args)
    p1 = car(args)
    p2 = cdr(args)
    if _eval(p1) != @o_man.nil
      return func_progn(p2)
    end

    return @o_man.nil
  end

  # Syntax: (unless expr else-expr...)
  def func_unless(args)
    p1 = car(args)
    p2 = cdr(args)
    if _eval(p1) != nil
      return func_progn(p2)
    end

    return @o_man.nil
  end

  # Syntax: (while expr exprs...)
  def func_while(args)
    p1 = car(args)
    p2 = cdr(args)

    while _eval(p1) != @o_man.nil
      func_progn(p2)
    end

    return @o_man.nil
  end

  # Syntax: (progn expr...)
  def func_progn(args)
    p  = args
    p1 = nil

    loop do
      p1 = _eval(car(p))
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    return p1
  end

  # Syntax: (prog1 expr...)
  def func_prog1(args)
    p     = args
    first = nil

    loop do
      p1 = _eval(car(p))
      if !first
        first = p1
      end

      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    if !first
      first = @o_man.nil
    end

    return first
  end

  # Syntax: (prog2 expr...)
  def func_prog2(args)
    p      = args
    second = nil
    i      = 0

    loop do
      i += 1
      p1 = _eval(car(p))
      if i == 2
        second = p1
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    if !second
      second = @o_man.nil
    end

    return second
  end

  # Syntax: (prog2 expr...)
  def func_prog2(args)
    p      = args
    second = nil
    i      = 0
    
    loop do
      i += 1
      p1 = _eval(car(p))
      if i == 2
        second = p1
      end
      p = cdr(p)
      break unless (p != @o_man.nil)
    end

    if !second
      second = nil
    end

    return second
  end

  # Syntax: (set name value)
  def func_set(args)
    p1 = _eval(car(args))
    p2 = _eval(car(cdr(args)))

    if p1 == @o_man.nil
      if @lint
        Error.warn("warning: setting the value of a nil object")
      end
    else
      @o_man.set_object(p1, p2)
    end
    return p2
  end

  # Syntax: (setq name value...)
  # Syntax: (setf name value...)
  # `name' is not eval'd
  def func_setq(args)
    p = args
    p2 = nil

    loop do
      p1 = car(p)
      p2 = _eval(car(cdr(p)))
      @o_man.set_object(p1, p2)
      p = cdr(cdr(p))
      break unless (p != @o_man.nil)
    end

    return p2
  end


  # Syntax: (defun name arglist expr...)
  # `name' is not evalled
  # `arglist' is not evalled
  def func_defun(args)
    p1 = car(args)
    p2 = car(cdr(args))
    p3 = cdr(cdr(args))

    lexpr = @o_man.new_object(LObject::OBJ_CONS)
    lexpr.value.c.car = @o_man.new_object(LObject::OBJ_IDENTIFIER)
    lexpr.value.c.car.value.id = "lambda"
    lexpr.value.c.cdr = @o_man.new_object(LObject::OBJ_CONS)
    lexpr.value.c.cdr.value.c.car = p2
    lexpr.value.c.cdr.value.c.cdr = p3
    @o_man.set_object(p1, lexpr)
    return lexpr
  end

  def eval_func(p, args)
    p1        = car(p)
    eval_objs = []
    save_objs = []

    if p1.type == LObject::OBJ_IDENTIFIER and p1.value.id == "lambda"
      p2 = car(cdr(p))
      p3 = args

      if count_list(p2) != count_list(p3)
        Error.warn("warning: wong number of parameters")
        return @o_man.nil
      end

      # save the new variable names
      loop do
        p5 = _eval(car(p3))
        eval_objs << p5  
        p3 = cdr(p3)
        break unless (p3 != @o_man.nil)
      end

      # save the old variable values and set the new ones
      i = 0
      loop do
        p4 = car(p2)
        save_objs << @o_man.get_object(p4)
        @o_man.set_object(p4, eval_objs[i])
        p2 = cdr(p2)
        i += 1
        break unless (p2 != @o_man.nil)
      end

      p5 = func_progn(cdr(cdr(p)))

      # restore the old variable values
      p2 = car(cdr(p))
      i  = 0
      loop do
        p4 = car(p2)
        @o_man.set_object(p4, save_objs[i])
        i += 1
        p2 = cdr(p2)
        break unless (p2 != @o_man.nil)
      end
      return p5
    end

    return @o_man.nil
  end

  def func_gc(args)
    @o_man.garbage_collect()
    return @o_man.t
  end

  def func_dump_memory(args)
    p = car(args)

    if p != @o_man.nil and cdr(args) == @o_man.nil and p.type == LObject::OBJ_STRING
      if p.value.s.length > 0
        dump_objects(p.value.s)
        return @o_man.t
      else
        Error.warn("expected filename")
      end
    else
      Error.warn("wrong number of parameters (expected string)")
    end
    return @o_man.nil
  end

  def eval_cons(p)
    p1 = car(p)
    p2 = cdr(p)

    if p1 != @o_man.nil and p1.type == LObject::OBJ_IDENTIFIER
      if p1.value.id == "lambda"
        return p
      end

      if @intrinsics.has_key?(p1.value.id)
        return send(@intrinsics[p1.value.id], p2) 
      end

      if (p3 = @o_man.get_object(p1)) != @o_man.nil
        return eval_func(p3, p2)
      else
        Error.warn("warning: function `%s' is undefined", p1.value.id)
      end
    end

    return @o_man.nil
  end

  def _eval(p)
    case p.type
      when LObject::OBJ_IDENTIFIER
        return @o_man.get_object(p)
      when LObject::OBJ_INTEGER
      when LObject::OBJ_STRING
        return p
      when LObject::OBJ_CONS
        return eval_cons(p)
    end
    return p
  end

  def func_eval(args)
    return _eval(_eval(car(args)))
  end
end
