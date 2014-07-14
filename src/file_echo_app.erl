-module(file_echo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, echo_file/2, echo_files/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _Args) ->
    file_echo_sup:start_link().

stop(_State) ->
    ok.


echo_file(Filename, Mode) ->
	file_reader:echo_file(file_reader, Filename, Mode).

echo_files(Mode) ->
	{ok, NumReaders} = application:get_env(file_echo, num_readers),
	lists:foreach(fun(Num) ->
			ProcessName = list_to_atom("reader_" ++ integer_to_list(Num)),
			spawn(fun() ->
				file_reader:echo_file(ProcessName, integer_to_list(Num)++".txt", Mode)
			end)
		end,
		lists:seq(1, NumReaders)
	).






