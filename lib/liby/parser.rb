require 'liby/error'
require 'liby/lexer'
require 'liby/lobject'
require 'liby/objectmanager'

class Parser
  def initialize(om, input_file)
    @lexer     = Lexer.new(input_file)
    @thistoken = ''
    @om        = om
  end

  def parse_form
    p     = LObject.new
    first = nil
    prev  = nil

    while (@thistoken = @lexer.gettoken) != ')' && @thistoken != LObject::EOF
      if @thistoken == '.'
        @thistoken = @lexer.gettoken
        if prev.nil?
          Error.error(1, "unexpected `.'")
        end
        prev.value.c.cdr = parse_object(1)

        if (@thistoken = @lexer.gettoken) != ')'
          Error.error(1, "expected `)'")
        end
        break
      end

      p = @om.new_object(LObject::OBJ_CONS)
      first = p unless first
      if prev
        prev.value.c.cdr = p
      end

      p.value.c.car = parse_object(1)
      prev = p
    end

    if not first
      return @om.nil
    else
      return first
    end
  end

  def parse_quote
    p = @om.new_object(LObject::OBJ_CONS)
    p.value.c.car = @om.new_object(LObject::OBJ_IDENTIFIER)
    p.value.c.car.value.id = "quote"
    p.value.c.cdr = @om.new_object(LObject::OBJ_CONS)
    p.value.c.cdr.value.c.car = parse_object(0)
    return p
  end

  def parse_object(havetoken)
    p = nil

    if havetoken.zero?
      @thistoken = @lexer.gettoken
    end

    case @thistoken
      when LObject::EOF
      when "("
        p = parse_form()
      when "'"
        p = parse_quote()
      when LObject::IDENTIFIER
        if @lexer.token_buffer == "t"
          p = @om.t
        elsif @lexer.token_buffer == "nil"
          p = @om.nil
        else
          if (p = @om.search_object_identifier(@lexer.token_buffer)) == nil
            p = @om.new_object(LObject::OBJ_IDENTIFIER)
            p.value.id = @lexer.token_buffer.clone
          end
        end
      when LObject::INTEGER
        i = @lexer.token_buffer.to_i
        if (p = @om.search_object_integer(i)) == nil
          p = @om.new_object(LObject::OBJ_INTEGER)
          p.value.i = i
        end
      when LObject::STRING
        if (p = @om.search_object_string(@lexer.token_buffer)) == nil
          p = @om.new_object(LObject::OBJ_STRING)
          p.value.s = @lexer.token_buffer.clone
        end
      else
        Error.warn("%d: unexpected character `%c'", @lexer.lineno, @thistoken)
    end

    return p
  end
end
