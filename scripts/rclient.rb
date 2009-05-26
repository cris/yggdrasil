require 'rubygems'
require 'json'
require 'socket'

class RClient
	MAX_SIZE = 4096

	def initialize(host, port)
		@host, @port = host, port
		@socket = TCPSocket.new(host, port)
	end

	def send(data)
		@socket.write([data.size].pack("N"))
		@socket.write(data)
	end

	def read
		size = @socket.readpartial(4).unpack("N").first.to_i
		@socket.readpartial(size)
	end

	def commands
		puts "1. login"
	end

	def get_command(name)
		command_map = case name
									when 'login'
										{:verb => "PUT", :resource => "/world/actors",
										 #:headers => {},
										 :params => {:login => "user", :password => "pass"}}
									when ''
									end
		command_map.to_json
	end

	def ask_for_command
		commands
		print "Enter: "
		command = gets.strip
	end

	def execute(command)
		command_string = get_command(command)
		puts command_string
		self.send(command_string)
		value = self.read
		puts "Size: #{value.size}"
		puts "Echo: #{value}"
	end

	def send_loop
		while true
			command = ask_for_command
			execute(command)
		end
	rescue EOFError
		puts "Closed"
		exit
	end

	class Message
		class << self
			# Exit
			def error_exit(message="")
				error(message)
				exit 1
			end

			def ok_exit(message="")
				ok(message)
				exit 0
			end
			# Formatted messages
			def header(message="")
				size = 76
				star_length = (size - message.size) / 2
				puts "\n#{'*' * star_length}  #{message}  #{'*' * star_length}\n"
			end

			# colorized output ok and not ok =) 
			def ok(message="")
				puts "[ #{green('Ok')} ] #{message}"
			end

			def info(message="")
				puts "[ #{green('Info')} ] #{message}"
			end

			def warning(message="")
				puts "[ #{yellow('Warning')} ] #{message}"
			end

			def tip(message="")
				puts "[ #{yellow('Tip')} ] #{message}"
			end

			def usage(message="")
				puts "[ #{yellow('Usage')} ] #{message}"
			end

			def error(message="")
				puts "[ #{red('Error')} ] #{message}"
			end

			#colorize output
			def colorize(text, color_code)
				"#{color_code}#{text}\e[0m"
			end
			def red(text)
				colorize(text, "\e[31m")
			end
			def green(text)
				colorize(text, "\e[32m")
			end
			def yellow(text)
				colorize(text, "\e[33m")
			end
		end
	end
end

RClient.new("localhost", 4990).send_loop
