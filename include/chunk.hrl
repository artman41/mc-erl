-record(chunk_layer, {
    y_position :: integer(),
    types :: binary(), 
    metadata :: binary(), 
    block_light :: binary(), 
    sky_light :: binary()
}).

-record(chunk_data, {
    layers :: list(#chunk_layer{}),
    biome :: binary()
}).

-record(chunk, {
    position :: {X :: integer(), Y :: integer()},
    data :: #chunk_data{}
}).