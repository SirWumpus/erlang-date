-module(edate_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([
	test_usage/1, test_default_output/1,
	test_option_d/1, test_option_d_u/1, test_option_d_error/1,
	test_option_u/1,
	test_option_r/1
]).

-define(DUMP, ?MIN_IMPORTANCE).
-define(DEBUG, ?LOW_IMPORTANCE).
-define(INFO, ?STD_IMPORTANCE).
-define(WARN, ?HI_IMPORTANCE).
-define(ERROR, ?MAX_IMPORTANCE).

all() ->
	[
	test_usage, test_default_output,
	test_option_d, test_option_d_error,
	test_option_u, test_option_r
	].

edate(Args) ->
	Out = str:trim(list_to_binary(os:cmd(["../../bin/edate ", Args]))),
	ct:pal(?DEBUG, "~s", [Out]),
	str:trim(Out).

test_usage(_Config) ->
	Out = edate("-?"),
	0 = str:str(Out, <<"unknown option -?">>),
	-1 /= str:str(Out, <<"usage: edate">>).

test_default_output(_Config) ->
	Out = edate(""),
	{ExpectDate, ExpectTime} = calendar:local_time(),
	ExpectTz = dtz:time_zone_seconds(),
	{{GotDate, GotTime, GotTz}, _} = str:to_date_time(Out),
	ct:pal(?DEBUG, "expect ~w, got ~w", [
		{ExpectDate, ExpectTime, ExpectTz}, {GotDate, GotTime, GotTz}
	]),
	{ExpectDate, ExpectTime, ExpectTz} = {GotDate, GotTime, GotTz}.

test_option_d(_Config) ->
	Out = edate("-d '1 Apr 2017' '+%s'"),
	<<"1491019200">> = Out,
	{{{2017, 4, 1}, {0, 0, 0}, -14400}, <<>>} = str:ptime(Out, <<"%s">>).

test_option_d_u(_Config) ->
	Out = edate("-u -d '1 Apr 2017' '+%s'"),
	<<"1491019200">> = Out,
	{{{2017, 4, 1}, {4, 0, 0}, 0}, <<>>} = str:ptime(Out, <<"%s">>).

test_option_d_error(_Config) ->
	Out = edate("-d foobar"),
	-1 /= str:str(Out, <<"edate: cannot parse foobar">>).

test_option_u(_Config) ->
	Out = edate("-u -d '13:11 -0400'"),
	-1 /= str:str(Out, <<"17:11:00 +0000">>).

test_option_r(_Config) ->
	UtcSeconds = edate("-d '8 Apr 2017 15 30 -0330' '+%s'"),
	UtcDate = edate(["-u -r" | binary_to_list(UtcSeconds)]),
	-1 /= str:str(UtcDate, <<"Sat,  8 Apr 2017 19:00:00 +0000">>).
