%% Copyright (C) 2009 Romuald du Song <rdusong _AT_ gmail _DOT_ com>.
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(xmlrpc_tests).
-export([handler/2,start/0]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
    {"The server can be started, stopped",
     ?setup(fun is_alive/1)}.

variables_types_test_() ->
    [{"A simple int value is be a valid parameter type",
     ?setup(fun handle_int_call/0)},
     {"A simple float value is a valid parameter type",
      ?setup(fun handle_float_call/0)},
     {"A complex struct value is a valid parameter type",
      ?setup(fun handle_struct_call/0)}
     ].

timeout_call_test_() ->
    [{"A timeout call time can be made",
     ?setup(fun handle_timeout_call/0)}].

is_alive(Pid) ->
    [?_assert(erlang:is_process_alive(Pid))].

handle_int_call() ->
    ?_assert(xmlrpc:call(localhost, 4567, "/",
                        {call, echo, [42]}) =:= {ok,{response,[{array, [42]}]}}).

handle_float_call() ->
    ?_assert(xmlrpc:call(localhost, 4567, "/",
                        {call, echo, [42.0]}) =:= {ok,{response,[{array, [42.0000]}]}}).

handle_struct_call() ->
    ?_assert(xmlrpc:call({127, 0, 0, 1}, 4567, "/",
                        {call, echo, [2.6,
                                      {array, [5, "foo"]},
                                      {struct,
                                       [{baz, 1},
                                        {bar, {base64, "aXMgdGhpcyBiaW5hcnkgPw=="}}]
                                      }]}) =:=
            {ok,{response,[{array,[2.60000,
                                   {array,[5,"foo"]},
                                   {struct,
                                    [{baz,1},
                                     {bar,{base64,"aXMgdGhpcyBiaW5hcnkgPw=="}}]
                                   }]}]}}).

handle_timeout_call() ->
    ?_assert(xmlrpc:call(localhost, 4567, "/",
                        {call, echo, [42]}, true, 10000) =:= {ok,{response,[{array, [42]}]}}).

%% setup functions

%% start local XMLRPC echo server for tests
start() ->
    {ok, Pid} = xmlrpc:start_link({?MODULE, handler}),
    Pid.

stop(Pid) ->
    xmlrpc:stop(Pid).

handler(_, {call, echo, Params}) ->
    {false, {response, [{array, Params}]}};
handler(_, Payload) ->
    FaultString = lists:flatten(io_lib:format("Unknown call: ~p", [Payload])),
    {false, {response, {fault, -1, FaultString}}}.

