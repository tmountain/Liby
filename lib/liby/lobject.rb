# like a fine wine, it gets better with age
class ConsCell
  attr_accessor :car, :cdr
end

# used as a convenience to match scoping of reference implementation
class Attributes
  attr_accessor :id, :s, :i, :c

  def initialize
    @c = ConsCell.new
  end
end

# lambda calculus workhorse (LObject <- Attributes <- ConsCell)
class LObject
  attr_accessor :type, :gc, :value, :next

  # Tokens
  EOF        = -1
  IDENTIFIER = 1
  INTEGER    = 2
  STRING     = 3

  # Lisp objects
  OBJ_NIL        = 0
  OBJ_T          = 1
  OBJ_INTEGER    = 10
  OBJ_IDENTIFIER = 11
  OBJ_STRING     = 12
  OBJ_CONS       = 20

  def initialize
    @value = Attributes.new
  end
end

class ObjectPair
  attr_accessor :name, :value, :next
end
