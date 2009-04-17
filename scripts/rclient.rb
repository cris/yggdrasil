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

	def send_loop
		while true
			print "Enter: "
			rvalue = gets.strip
			self.send(rvalue)
			value = self.read
			puts "Size: #{value.size}"
			puts "Echo: #{value}"
		end
	rescue EOFError
		puts "Closed"
	end
end

RClient.new("localhost", 4990).send_loop
