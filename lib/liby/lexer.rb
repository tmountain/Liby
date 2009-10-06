require 'liby/lobject'

class IO
  def getchr
    return (c = self.getc) ? c.chr : LObject::EOF
  end
end

class String
  def isdigit?
    return (self[0] >= 48 and self[0] <= 57) ? true : false
  end
end

class Lexer
  attr_reader :lineno, :token_buffer
  def initialize(file)
    @lineno       = 1
    @file         = file
    @token_buffer = String.new
  end

  # fetch next token from the input stream
  def gettoken
    loop do
      c = @file.getchr

      catch(:redo) do
        case c
          # track lineno for error output
          when "\n"
            @lineno += 1

          # whitespace
          when /[\s\f\t\v\r]/

          # comment of the form /;.*\n/
          when ";"
            while (c = @file.getchr) != "\n"; end
            @lineno += 1

          # ascii representation of adjacent char
          when "?"
            c = @file.getchr
            @token_buffer = c.to_s
            return LObject::INTEGER

          # minus symbol
          when "-"
            c = @file.getchr

            if !c.isdigit?
              @file.ungetc(c[0])
              c = :identifier
              redo
            else
              # push the digit back into the stream
              @file.ungetc(c[0])
              c = :negative
              redo
            end

          # numeric literal
          when /[0-9]/, :negative
            # if negative symbol was set, add a leading minus sign
            c = (c == :negative) ? "-" : c

            @token_buffer = c
            while (c = @file.getchr).isdigit?
              @token_buffer += c
            end
            @file.ungetc(c[0])
            return LObject::INTEGER

          # identifier [-/+*%<>=&a-zA-Z_][-/+*%<>=&a-zA-Z_0-9]*
          when %r{[_+*/%<>=&a-zA-Z]}, :identifier
            # if identifier was set, we came here from minus pattern
            c = (c == :identifier) ? "-" : c
            @token_buffer = c

            #loop do
            #  c = @file.getchr
            #  break unless c =~ %r{[-_+*/%<>=&a-zA-Z0-9]}
            #end
            while (c = @file.getchr) =~ %r{[-_+*/%<>=&a-zA-Z0-9]}
              @token_buffer += c
            end

            @file.ungetc(c[0])
            return LObject::IDENTIFIER

          # string with support for control characters
          when '"'
            @token_buffer = String.new
            while (c = @file.getchr) != '"' and c != LObject::EOF
              # handle escape sequences 
              if c == "\\"
                c = @file.getchr
                case c
                  when "\n"
                    @lineno += 1
                  when "a"
                    @token_buffer += "\a"
                  when "b"
                    @token_buffer += "\b"
                  when "f"
                    @token_buffer += "\f"
                  when "n"
                    @token_buffer += "\n"
                  when "r"
                    @token_buffer += "\r"
                  when "t"
                    @token_buffer += "\t"
                  when "v"
                    @token_buffer += "\v"
                  else
                    @token_buffer += c
                end
              else
                if c == "\n"
                  @lineno += 1
                end
                @token_buffer += c
              end
            end
            return LObject::STRING

          # default (return single char)
          else
            return c
        end
      end
    end
  end
end
