-module(edate_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0]).
-export([usage/1, default_output/1]).

-define(DUMP, ?MIN_IMPORTANCE).
-define(DEBUG, ?LOW_IMPORTANCE).
-define(INFO, ?STD_IMPORTANCE).
-define(WARN, ?HI_IMPORTANCE).
-define(ERROR, ?MAX_IMPORTANCE).

all() ->
	[usage, default_output].

edate(Args) ->
	Out = list_to_binary(os:cmd(["../../bin/edate ", Args])),
	ct:pal(?DEBUG, "~s", [Out]),
	Out.

usage(_Config) ->
	Out = edate("-?"),
	0 = str:str(Out, <<"unknown option -?">>),
	Expect = str:chr(Out, $\n) + 1,
	Got = str:str(Out, <<"usage: edate">>),
	ct:pal(?DEBUG, "expect ~B, got ~B", [Expect, Got]),
	Expect = Got.

default_output(_Config) ->
	Out = edate(""),
	{ExpectDate, ExpectTime} = calendar:local_time(),
	ExpectTz = dtz:time_zone_seconds(),
	{{GotDate, GotTime, GotTz}, _} = str:to_date_time(Out),
	ct:pal(?DEBUG, "expect ~w, got ~w", [
		{ExpectDate, ExpectTime, ExpectTz}, {GotDate, GotTime, GotTz}
	]),
	{ExpectDate, ExpectTime, ExpectTz} = {GotDate, GotTime, GotTz}.
