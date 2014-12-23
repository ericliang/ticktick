-define(ERROR(Format, Args), error_logger:error_msg(Format, Args)).
-define(INFO(Format, Args), error_logger:info_msg(Format, Args)).
-define(DEBUG(Format, Args), error_logger:info_msg(Format, Args)).

-record(ttid, { version,  %% 2bit
				seconds,  %% 30bit, in seconds
				mseconds, %% 10bit, in milliseconds
				sequence, %% 10bit
				machine,  %% 10bit, machine id, upto 1024
				tag       %% 2bit, normal 01, group 10
			  }).

-define(TTID_TIME_BEGIN, 1417564800). %% seconds diff to 20141203 00:00:00 GMT
-define(TTID_SEQ_MAX, 1024).
-define(TTID_TAG_NORMAL, 0).
-define(TTID_BIN_SIZE, 64).
