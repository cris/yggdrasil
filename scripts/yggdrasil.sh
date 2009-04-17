#!/bin/sh

ledit -x -h ~/.erl_history erl \
	-pa "`dirname $0`/../ebin" \
	-s yggdrasil \
	-sname yggdrasil \
	-boot start_sasl \
	-sasl sasl_error_logger '{file,"/var/log/yggdrasil/sasl.log"}' \
	-kernel error_logger '{file,"/var/log/yggdrasil/yggdrasil.log"}' \
	+W w
