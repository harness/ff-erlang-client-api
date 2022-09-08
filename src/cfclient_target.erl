-module(cfclient_target).

-export([encode/1]).

-export_type([cfclient_target/0]).

-type cfclient_target() ::
    #{ 'identifier' := binary(),
       'account' := binary(),
       'org' := binary(),
       'environment' := binary(),
       'project' := binary(),
       'name' := binary(),
       'anonymous' => boolean(),
       'attributes' => maps:map(),
       'createdAt' => integer(),
       'segments' => list()
     }.

encode(#{ 'identifier' := Identifier,
          'account' := Account,
          'org' := Org,
          'environment' := Environment,
          'project' := Project,
          'name' := Name,
          'anonymous' := Anonymous,
          'attributes' := Attributes,
          'createdAt' := CreatedAt,
          'segments' := Segments
        }) ->
    #{ 'identifier' => Identifier,
       'account' => Account,
       'org' => Org,
       'environment' => Environment,
       'project' => Project,
       'name' => Name,
       'anonymous' => Anonymous,
       'attributes' => Attributes,
       'createdAt' => CreatedAt,
       'segments' => Segments
     }.
