-module(queue_demo).
%%----------------------------------------------------------%%
-export([publish_msg/1]).
%%----------------------------------------------------------%%
-spec publish_msg(#{}) -> no_return().
publish_msg(Payload) ->
    queue_demo_server:publish_msg(jsx:encode(Payload)).