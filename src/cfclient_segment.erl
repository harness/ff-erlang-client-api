-module(cfclient_segment).

-export([encode/1]).

-export_type([cfclient_segment/0]).

-type cfclient_segment() ::
    #{ 'identifier' := binary(),
       'name' := binary(),
       'environment' => binary(),
       'tags' => list(),
       'included' => list(),
       'excluded' => list(),
       'rules' => list(),
       'createdAt' => integer(),
       'modifiedAt' => integer(),
       'version' => integer()
     }.

encode(#{ 'identifier' := Identifier,
          'name' := Name,
          'environment' := Environment,
          'tags' := Tags,
          'included' := Included,
          'excluded' := Excluded,
          'rules' := Rules,
          'createdAt' := CreatedAt,
          'modifiedAt' := ModifiedAt,
          'version' := Version
        }) ->
    #{ 'identifier' => Identifier,
       'name' => Name,
       'environment' => Environment,
       'tags' => Tags,
       'included' => Included,
       'excluded' => Excluded,
       'rules' => Rules,
       'createdAt' => CreatedAt,
       'modifiedAt' => ModifiedAt,
       'version' => Version
     }.
