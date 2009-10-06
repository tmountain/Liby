require 'liby/error'
require 'liby/lobject'

# set some constants on allowable limits for this platform
class Fixnum
  N_BYTES = [42].pack('i').size
  N_BITS  = N_BYTES * 8
  MAX     = 2 ** (N_BITS - 2) - 1
  MIN     = -MAX - 1
end

class ObjectManager
  attr_accessor :nil, :t
  attr_reader :used_objs, :free_objs
  def initialize
    @nil            = LObject.new
    @t              = LObject.new
    @gc_id          = 0 # used for tagging lisp objects
    @free_objs_list = nil
    @used_objs_list = nil
    @setobjs_list   = nil
    @free_objs      = 0
    @used_objs      = 0
  end

  def new_object(type)
    p = nil

    if @free_objs_list.nil?
      p = LObject.new
    else
      p = @free_objs_list
      @free_objs_list = @free_objs_list.next
      @free_objs -= 1
    end

    p.next = @used_objs_list
    @used_objs_list = p
    p.type = type

    if type == LObject::OBJ_CONS
      p.value.c.car = @nil
      p.value.c.cdr = @nil
    end

    p.gc = 0
    @used_objs += 1
    return p
  end

  def search_object_identifier(s)
    p = @used_objs_list

    while p
      if p.type == LObject::OBJ_IDENTIFIER and p.value.id == s
        return p
      end
      p = p.next
    end
    return nil
  end


  def search_object_string(s)
    p = @used_objs_list

    while p
      if p.type == LObject::OBJ_STRING and p.value.s == s
        return p
      end
      p = p.next
    end
    return nil
  end

  def search_object_integer(i)
    p = @used_objs_list

    while p
      if p.type == LObject::OBJ_INTEGER and p.value.i == i
        return p
      end
      p = p.next
    end
    return nil
  end

  def init_objects
    @nil = new_object(LObject::OBJ_NIL)
    @t = new_object(LObject::OBJ_T)
  end

  def set_object(name, value)
    return if !name.value.id

    p = @setobjs_list
    while p
      if p.name.value.id and name.value.id == p.name.value.id
        p.value = value
        return
      end
      p = p.next
    end

    p = ObjectPair.new
    p.next = @setobjs_list
    @setobjs_list = p
    p.name = name
    p.value = value
  end

  def get_object(name)
    p = @setobjs_list
    while p
      if p.name.value.id and name.value.id == p.name.value.id
        return p.value
      end
      p = p.next
    end
    return @nil
  end

  def dump_objects(fname)
    fout = nil

    begin
      fout = File.new(fname, "w") 
    rescue
      Error.error(1, "%s", fname)
    end

    p = @setobjs_list
    while p
      fout.printf("(setq %s '", p.name.value.id)
      fout.print(p.value.inspect)
      fout.puts(")")
      p = p.next
    end
    fout.close
  end

  def obj_type_str(p)
    return unless Error.debug
    case p.type
      when LObject::OBJ_NIL
        return "nil"
      when LObject::OBJ_T
        return "t"
      when LObject::OBJ_INTEGER
        return "integer"
      when LObject::OBJ_STRING
        return "string"
      when LObject::OBJ_IDENTIFIER
        return "identifier"
      when LObject::OBJ_CONS
        return "cons"
      else
        raise "Invalid object type encountered."
    end
  end

  def print_obj_lists
    return unless Error.debug
    Error::warn(":: used objects")
    p = @used_objs_list

    while p
      Error::warn("::  %p (%s)", p.object_id, obj_type_str(p))
      p = p.next
    end

    Error::warn(":: free objects")
    p = @free_objs_list
    while p
      Error::warn("::   %p (%s)", p.object_id, obj_type_str(p))
      p = p.next
    end
  end

  # garbage collection routines
  def tag_tree(p)
    if p.gc == @gc_id
      return
    end

    p.gc = @gc_id
    if p.type == LObject::OBJ_CONS
      tag_tree(p.value.c.car)
      tag_tree(p.value.c.cdr)
    end
  end

  def tag_whole_tree
    p = @setobjs_list
    while p
      tag_tree(p.name)
      tag_tree(p.value)
      p = p.next
    end
  end

  def do_garbage_collect
    new_used_objs_list = @t
    tag_whole_tree()
    p = @used_objs_list
    _next = LObject.new

    while p and p != @t
      _next = p.next
      if p.gc != @gc_id
        Error.warn(":: collecting cons %p", p.object_id)

        case p.type
          when LObject::OBJ_STRING
            p.value.s = nil
          when LObject::OBJ_IDENTIFIER
            p.value.id = nil
        end
        
        p.next = @free_objs_list
        @free_objs_list = p
        @free_objs += 1
        @used_objs -= 1
      else
        p.next = new_used_objs_list
        new_used_objs_list = p
      end
      p = _next
    end
    @used_objs_list = new_used_objs_list
    GC.start
  end

  def garbage_collect
    if (@gc_id += 1) == Fixnum::MAX
      @gc_id = 1
    end
    do_garbage_collect()
  end
end
