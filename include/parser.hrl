-define(IS_WORD_CHAR(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) orelse (C =:= $_) orelse (C >= $0 andalso $ =< $9))).

-define(IS_LWORD_CHAR(C), ((C >= $a andalso C =< $z) orelse (C =:= $_) orelse (C >= $0 andalso $ =< $9))).

-define(IS_NUMBER_CHAR(C), (C >= $0 andalso C =< $9)).

-define(IS_SPACE(C), (C =:= $\ )).

