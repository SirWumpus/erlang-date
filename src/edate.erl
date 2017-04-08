%%#!/usr/bin/env escript

-module(edate).
-export([main/1]).

-define(BUFSIZ, 1024).

usage() ->
	io:format("usage: edate [-d date | -r seconds] [+format]~n"),
	io:format("-d date\t\tparse date and/or time string~n"),
	io:format("-r seconds\tUTC seconds from the Epoch~n"),
	io:format("+format\t\tstr:ftime/2 format string~n"),
	halt(2).

main(Args) ->
	case egetopt:parse(Args, [
		{ $d, param, parse_date },
		{ $r, param, epoch_seconds }
	]) of
	{ok, Options, ArgsN} ->
		{DateTimeTz, _} = case proplists:get_value(parse_date, Options) of
		undefined ->
			case proplists:get_value(epoch_seconds, Options) of
			undefined ->
				{Date, Time} = calendar:local_time(),
				{{Date, Time, dtz:time_zone_seconds()}, undefined};
			UTC ->
				str:ptime(list_to_binary(UTC), <<"%s">>)
			end;
		ParseDate ->
			case str:to_date_time(list_to_binary(ParseDate)) of
			badarg ->
				io:format("edate: cannot parse ~s~n", [ParseDate]),
				halt(1);
			Result ->
				Result
			end
		end,

		Format = case ArgsN of
		[] ->
			<<"%a, %c %z">>;
		[[$+ | Fmt] | _] ->
			list_to_binary(Fmt)
		end,

		io:format("~s~n", [str:ftime(Format, DateTimeTz)]);
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage()
	end.
