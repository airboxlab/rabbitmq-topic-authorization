%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Topic Authorization.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_topic_authorization).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-rabbit_boot_step({?MODULE,
                   [{description, "topic-based authorization"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"topic-based authorization">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"topic-based authorization">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

-define(MAX_PERMISSION_CACHE_SIZE, 12).

init(Ch) ->
    {rabbit_channel:get_vhost(Ch), rabbit_channel:get_user(Ch)}.

description() ->
    [{description,
      <<"Checks authorization based on routing keys">>}].

check_resource_access(User, Resource, Perm) ->
	V = {Resource, Perm},
    Cache = case get(permission_cache) of
                undefined -> [];
                Other     -> Other
            end,
    case lists:member(V, Cache) of
        true  -> ok;
        false -> ok = rabbit_access_control:check_resource_access(
                        User, Resource, Perm),
                 CacheTail = lists:sublist(Cache, ?MAX_PERMISSION_CACHE_SIZE-1),
                 put(permission_cache, [V | CacheTail])
    end.

check_write_permitted(Resource, User) ->
    check_resource_access(User, Resource, write).

check_read_permitted(Resource, User) ->
    check_resource_access(User, Resource, read).

intercept(#'basic.publish'{routing_key = RoutingKeyBin} = Method, Content, State) ->
	RoutingKey = rk(State, RoutingKeyBin),
	check_write_permitted(RoutingKey, element(2, State)),
    {Method, Content};

intercept(#'exchange.bind'{routing_key = RoutingKeyBin} = Method, Content, State) ->
    RoutingKey = rk(State, RoutingKeyBin),
    check_read_permitted(RoutingKey, element(2, State)),
    {Method, Content};

intercept(#'queue.bind'{routing_key = RoutingKeyBin} = Method, Content, State) ->
	RoutingKey = rk(State, RoutingKeyBin),
	check_read_permitted(RoutingKey, element(2, State)),
    {Method, Content};

intercept(Method, Content, _State) ->
    {Method, Content}.

applies_to() -> 
	['basic.publish'].

%%----------------------------------------------------------------------------

rk(State, RKBin) ->
	rabbit_misc:r(element(1, State), routing_key, RKBin).