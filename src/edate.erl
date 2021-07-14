%%#!/usr/bin/env escript

-module(edate).
-export([main/1]).

-define(BUFSIZ, 1024).

usage() ->
	io:format("usage: edate [-u][-d date | -r seconds] [+format]~n"),
	io:format("-d date\t\tparse date and/or time string~n"),
	io:format("-r seconds\tUTC seconds from the Epoch~n"),
	io:format("-u\t\tdisplay UTC~n"),
	io:format("+format\t\tstr:ftime/2 format string~n"),
	halt(2).

main(Args) ->
	case opts:to_map(Args, [
		{ $d, param, parse_date },
		{ $r, param, epoch_seconds },
		{ $u, flag, utc_time }
	]) of
	{error, Reason, Opt} ->
		io:format("~s -~c~n", [Reason, Opt]),
		usage();
	{ok, _Options, ArgsN} ->
		process(ArgsN)
	end.

process(ArgsN) ->
	ShiftTime = case opts:get(utc_time, false) of
	true ->
		fun (DTZ) -> dtz:to_utc(DTZ) end;
	false ->
		fun (DTZ) -> DTZ end
	end,

	{DateTimeTz, _Rest} = case opts:get(parse_date) of
	undefined ->
		case opts:get(epoch_seconds) of
		undefined ->
			{Date, Time} = calendar:local_time(),
			{{Date, Time, dtz:time_zone_seconds()}, <<>>};
		Esecs ->
			str:ptime(list_to_binary(Esecs), <<"%s">>)
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
		% This is not historical nor POSIX, but practical.
		<<"%a, %c %z">>;
	[[$+ | Fmt] | _] ->
		list_to_binary(Fmt)
	end,

	io:format("~s~n", [str:ftime(Format, ShiftTime(DateTimeTz))]).
