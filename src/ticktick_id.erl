-module(ticktick_id).
-author('eric.l.2046@gmail.com').
-behaviour(gen_server).

-export([start_link/1,
		 stop/0,
		 id/0,
		 id_hex/0,
		 sibling/1,
         set_machine_id/1,
		 explain/1]).

-export([init/1,
		handle_call/3,
		handle_cast/2,
		code_change/3,
		handle_info/2,
		terminate/2]).

-include("ticktick.hrl").

-record(state, { version, machine_id, space_time, sequence }).
-define(PROCNAME, ?MODULE).
-define(TTID_VERSION, 0).

%% API

start_link(MachineId) ->
	gen_server:start_link({local, ?PROCNAME}, ?MODULE, [MachineId], []).

stop() ->
	gen_server:cast(?PROCNAME, stop).

id() ->
	gen_server:call(?PROCNAME, id).

set_machine_id(MachineId) ->
    gen_server:call(?PROCNAME, {machine_id, MachineId}).

id_hex() ->
	{ok, IdBin} = id(),
	hex:bin_to_hexstr(IdBin).

sibling( IdBin ) ->
	%% simply retrieve the next related id at the same time of Id,
	%% or error on last available id in same space(time)
	case bin_to_ttid( IdBin) of
		{ok, TTID} when is_record(TTID, ttid)->
			Seq = TTID#ttid.sequence,
			case Seq >= ?TTID_SEQ_MAX of
				true ->
					{error, exhausted};
				_ ->
					TTID1 = TTID#ttid{ sequence = Seq + 1 },
					ttid_to_bin(TTID1)
			end;
		Error ->
			Error
	end.

explain( IdBin ) ->
	%% return {ok, TTID} in which TTID is a ttid record, or error on invalid string
	{ok, TTID} = bin_to_ttid( IdBin ),
	Fields = record_info(fields, ttid),
	[_Tag| Values] = tuple_to_list(TTID),
	lists:zip(Fields, Values).

%% Internal

init([MachineId]) ->
	process_flag(trap_exit, true),
	?INFO("Ticktick running on machine : ~p ~n", [MachineId]),
	{ok, #state{ version = ?TTID_VERSION, machine_id = MachineId, 
				 space_time = erlang:now(), sequence = 0 }}.

handle_call(id, _From, #state{ space_time = SpaceTime, sequence = Seq } = State) ->	
	State1 = case timer:now_diff(Now = erlang:now(), SpaceTime) > 1000 of
				 true ->
					 %% next space state
					 State#state{space_time = Now, sequence = 0};
				 _ ->
					 State#state{sequence = Seq+1}
			 end,

	#state{ version = Ver, machine_id = MID, 
			space_time = SpaceTime1, 
			sequence = Seq1} = State1,

	case Seq1 >= ?TTID_SEQ_MAX of
		true ->
			{reply, {error, exhausted}, State};
		_ ->
			TTID = to_ttid( Ver, SpaceTime1, Seq1, MID, ?TTID_TAG_NORMAL),
			{reply, ttid_to_bin(TTID), State1}
	end;

handle_call({machine_id, MachineId}, _From, State) ->
    NewState = State#state{machine_id = MachineId},
    ?INFO("Ticktick set to run on macine : ~p ~n", [MachineId]),
    {reply, ok, NewState}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internals

to_ttid( Ver, Time, Seq, Mach, Tag ) ->
	{MegaSeconds, Seconds, MicroSeconds} = Time,
	#ttid{ version = Ver,
		   seconds = MegaSeconds*1000000 + Seconds - ?TTID_TIME_BEGIN,
		   mseconds = MicroSeconds div 1000,
		   sequence = Seq,
		   machine = Mach,
		   tag = Tag}.

ttid_to_bin( #ttid{ version = Ver, 
					seconds = Sec,
					mseconds = MSec,
					sequence = Seq,
					machine = Mach,
					tag = Tag } ) ->
	
	VerB = fix_size(binary:encode_unsigned(Ver), 2),
	SecB = fix_size(binary:encode_unsigned(Sec), 30),
	MSecB = fix_size(binary:encode_unsigned(MSec), 10),
	SeqB = fix_size(binary:encode_unsigned(Seq), 10),
	MachB = fix_size(binary:encode_unsigned(Mach), 10),
	TagB = fix_size(binary:encode_unsigned(Tag), 2),
	
	{ok, <<VerB/bits, SecB/bits, MSecB/bits, 
		   SeqB/bits, MachB/bits, TagB/bits>> }.

fix_size(Bin, Size) when is_integer(Size) andalso bit_size(Bin) =< Size ->
	%% padding
	PadSize = Size - bit_size(Bin),
	Bin1 = <<0:PadSize, Bin/bits>>,
	%% io:format("~p ~p ~n", [Bin, Bin1]),
	Bin1;
fix_size(Bin, Size) when is_integer(Size) ->
	%% truncating
	Size1 = bit_size(Bin) - Size,
	<<_:Size1, Bin1/bits>> = Bin,
	%% io:format("~w ~p ~w ~p ~n", [Bin, bit_size(Bin), Bin1, bit_size(Bin1)]),
	Bin1.

bin_to_ttid( IdBin ) ->
	case bit_size(IdBin) /= ?TTID_BIN_SIZE of
		true ->
			{error, invalid_bin};
		_ -> 
			<<VerB:2/bits, SecB:30/bits, 
			  MSecB:10/bits, SeqB:10/bits, 
			  MachB:10/bits, TagB:2/bits>> = IdBin,	
			TTID = #ttid{
					  version = bin_to_unsigned(VerB),
					  seconds = bin_to_unsigned(SecB),
					  mseconds = bin_to_unsigned(MSecB),
					  sequence = bin_to_unsigned(SeqB),
					  machine = bin_to_unsigned(MachB),
					  tag = bin_to_unsigned(TagB) },
			
			{ok, TTID}
	end.

bin_to_unsigned(Bin) ->
	Size = bit_size(Bin),
	Size1 = case Size rem 8 of
				 0 ->
					 Size;
				 _ ->
					 (Size div 8 + 1 ) * 8
			 end,
	Bin1 = fix_size(Bin, Size1),
	binary:decode_unsigned(Bin1).
	

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% function test 

overall_test_() ->
	{"Simple test on ticktick server.",
	 {setup, fun setup/0, fun cleanup/1,
	  {with, [ fun id_seq/1 ]}}
	}.

setup() ->
	start_link(123).

cleanup(_) ->
	stop().

id_seq(_) ->
	{ok, Id} = id(),
	{ok, Id1} = sibling(Id),

	{ok, TTID} = bin_to_ttid(Id),
	{ok, TTID1} = bin_to_ttid(Id1),
	io:format("~p ~p~n", [TTID, TTID1]),
	?assertEqual( TTID#ttid.sequence + 1, TTID1#ttid.sequence ).	

bits_size_test() ->	
	B = binary:encode_unsigned(987654321),
	[?assertEqual(bit_size(fix_size(B, 2)), 2),
	 ?assertEqual(bit_size(fix_size(B, 30)), 30)
	].

binary_id_conv_test() ->
	TTID = to_ttid( ?TTID_VERSION, erlang:now(), 0, 0, ?TTID_TAG_NORMAL),
	{ok, IdBin} = ttid_to_bin(TTID),
	io:format("IdBin: ~p~n", [IdBin]),
	{ok, TTID1} = bin_to_ttid(IdBin),
	[?assertEqual( TTID#ttid.version, TTID1#ttid.version ),
	 ?assertEqual( TTID#ttid.seconds, TTID1#ttid.seconds ),
	 ?assertEqual( TTID#ttid.mseconds, TTID1#ttid.mseconds ),
	 ?assertEqual( TTID#ttid.sequence, TTID1#ttid.sequence ),
	 ?assertEqual( TTID#ttid.machine, TTID1#ttid.machine ),
	 ?assertEqual( TTID#ttid.tag, TTID1#ttid.tag )
	].

-endif.
