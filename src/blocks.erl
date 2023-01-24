-module(blocks).

-include("block.hrl").

-compile(export_all).

-define(BLOCK(ModId, Name, Id, Placeable), #block{
    modid = ModId,
    name = Name,
    id = Id,
    placeable = Placeable
}).

-define(BLOCK(ModId, Name, Id), ?BLOCK(ModId, Name, Id, false)).

air() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 0 , true).
stone() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 1, true).
grass() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 2, true).
dirt() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 3, true).
cobblestone() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 4, true).
wooden_planks() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 5, true).
saplings() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 6, true).
bedrock() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 7, true).
sand() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 12, true).
gravel() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 13, true).
gold_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 14, true).
iron_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 15, true).
coal_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 16, true).
wood() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 17, true).
leaves() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 18, true).
sponge() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 19, true).
glass() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 20, true).
lapis_lazuli_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 21, true).
lapis_lazuli_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 22, true).
dispenser() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 23, true).
sandstone() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 24, true).
note_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 25, true).
powered_rail() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 27, true).
detector_rail() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 28, true).
sticky_piston() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 29, true).
cobweb() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 30, true).
tall_grass() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 31, true).
dead_bush() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 32, true).
piston() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 33, true).
wool() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 35, true).
dandelion() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 37, true).
rose() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 38, true).
brown_mushroom() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 39, true).
red_mushroom() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 40, true).
gold_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 41, true).
iron_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 42, true).
slabs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 44, true).
bricks() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 45, true).
tnt() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 46, true).
bookshelf() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 47, true).
moss_stone() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 48, true).
obsidian() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 49, true).
torch() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 50, true).
wooden_stairs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 53, true).
chest() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 54, true).
diamond_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 56, true).
diamond_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 57, true).
crafting_table() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 58, true).
furnace() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 61, true).
ladders() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 65, true).
rails() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 66, true).
cobblestone_stairs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 67, true).
lever() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 69, true).
stone_pressure_plate() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 70, true).
wooden_pressure_plate() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 72, true).
redstone_ore() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 73, true).
redstone_torch() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 76, true).
stone_button() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 77, true).
ice() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 79, true).
snow_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 80, true).
cactus() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 81, true).
clay_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 82, true).
jukebox() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 84, true).
fence() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 85, true).
pumpkin() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 86, true).
netherrack() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 87, true).
soul_sand() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 88, true).
glowstone_block() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 89, true).
jack_o_lantern() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 91, true).
trapdoor() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 96, true).
stone_bricks() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 98, true).
iron_bars() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 101, true).
glass_pane() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 102, true).
melon() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 103, true).
vines() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 106, true).
fence_gate() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 107, true).
brick_stairs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 108, true).
stone_brick_stairs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 109, true).
mycelium() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 110, true).
lily_pad() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 111, true).
nether_brick() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 112, true).
nether_brick_fence() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 113, true).
nether_brick_stairs() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 114, true).
enchantment_table() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 116, true).
end_stone() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 121, true).
dragon_egg() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 122, true).
redstone_lamp() -> ?BLOCK(minecraft, ?FUNCTION_NAME, 123, true).