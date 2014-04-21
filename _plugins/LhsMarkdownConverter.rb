module Jekyll
	class LhsMarkdownConverter < Converter
		safe true

		def initialize(config)
			@markdown = Converters::Markdown.new(config)
		end

		def matches(ext)
			ext =~ /^\.lhs$/i
		end

		def output_ext(ext)
			".markdown"
		end

		def render_markdown(markup)
			@markdown.convert(markup)
		end

		def render_highlighted(markup)
			require 'rouge'

			lexer = Rouge::Lexer.find_fancy("haskell", markup)
			formatter = Rouge::Formatters::HTML.new(linenos: false, wrap: false)

			pre = "<pre>#{formatter.format(lexer.lex(markup))}</pre>"
			return "<div class=\"highlight\">#{pre}</div>"
		end

		def convert(content)
			matches = /\[# links #\](.*?)\[# endlinks #\]/m.match(content)
			links = ""
			if matches then
				links = matches[1]
				content = matches.pre_match + matches.post_match
			end

			in_block = false
			out = ""
			buffer = ""

			content.lines.each do |l|
				l.freeze
				if l =~ /^[>\\]/ then
					if not in_block then
						buffer << links
						out << render_markdown(buffer)
						buffer = ""
					end
					in_block = true
					buffer << l.byteslice(2, l.bytesize - 2)
					next
				else
					if in_block then
						out << render_highlighted(buffer)
						buffer = ""
					end
					in_block = false
					buffer << l
					next
				end
			end

			if in_block then
				out << render_highlighted(buffer)
			else
				buffer << links
				out << render_markdown(buffer)
			end

			out
		end
	end
end