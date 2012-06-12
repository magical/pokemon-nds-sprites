
Sprites
=======

Sprites are stored in ``pokegra.narc``, but only Diamond and Pearl actually call it that. The later games, in an attempt to dissuade hackers, use a cryptic naming system. 

Diamond/Pearl/Platinum
----------------------

The main sprite file is ``/poketool/pokegra/pokegra.narc``. The alt sprite file is ``/poketool/pokegra/otherpoke.narc``.

The sprites in ``pokegra.narc`` are laid out in national dex order. They come in chunks of 6 files.

::

    0 NCGR - female back sprite
    1 NCGR - male   back sprite
    2 NCGR - female front sprite
    3 NCGR - male   front sprite
    4 NCLR - normal palette
    5 NCLR - shiny  palette

If the pokemon is male-only or genderless, the female sprites will be empty. If the pokemon is female-only the male sprites will be empty. Otherwise, all sprites will be full even in the (common) case where the male and female sprites do not differ.

The alternate forms are stored in ``otherpoke.narc``. The structure is 134 NCGR chunks followed by 74 NCLR chunks.

========= ==
Deoxys     4
Unown     28
Castform   4
Burmy      3
Wormadam   3
Shellos    2
Gastrodon  2
Cherrim    2
Arceus    18
========= ==

Deoxys and Unown each share a single normal and shiny palette between their forms; the others all have their own palettes.

The sprites are stored in back front back front interlacing order, except for Castform and Cherrim, which go back back front front, clumping all the backsprites and frontsprites together.

The last two sprites and palettes are for Egg and the Manaphy Egg, respectively.

Additionally, there are 3 NCGR chunks and 2 NCLR chunks at the very end of the file for the back and front Substitute doll sprites and the in-battle shadows.

HeartGold/SoulSilver
--------------------

The main sprite file is ``/a/0/0/4``. The alt sprite file is ``/a/1/1/4``.

There are additional alt sprites after the eggs.

========= ==
Shaymin    2
Rotom      6
Giratina   2
Pichu      2
========= ==

Black/White
-----------

Once again, the file is ``/a/0/0/4``. But the structure is completely different. Also, there is no ``otherpoke.narc`` - all the sprites are stored in the same file.

There are 20 files per pokemon (or form): 9 frontsprite-related files followed by 9 backsprite files followed by 2 palettes. The palettes are, of course, the normal and shiny palettes.

The front and back chunks look like this:

::

    0 NCGR male sprite
    1 NCGR female sprite
    2 NCGR male sprite parts
    3 NCGR female sprite parts
    4 NCER
    5 NANR
    6 NMCR
    7 NMAR
    8 ????

Unlike the previous games, the female sprites are empty if the Pokémon has no gender differences.

The first 2 files are simple static sprites. The "parts" files are 192x96 image files with all the body pieces of the pokemon laid out separately. One of the parts files is used together with the remaining files to animate the pokemon.

