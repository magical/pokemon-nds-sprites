
.. default-domain:: c
.. highlight:: c

C API
=====

Nitro
-----

.. c:function:: void *nitro_read(FILE *fp, off_t size)

    Read a nitro file from the filesystem.

    The ``size`` parameter is used when reading an LZSS_-compressed file.
    You may set it to 0 if the file you are loading isn't compressed.

    Returns ``NULL`` in the case of an error, or if the file type is unknown.

    .. note::

        NARC files are read lazily; they keep a copy of the file pointer to use when loading subfiles. Therefore, when reading a NARC file you should not close the file until you are done with the NARC.

    .. warning::

       Since this function returns a void pointer, it is *strongly* recommended that you check its type with :func:`nitro_get_magic` to make sure it is what you expected. Passing a struct of the wrong type to any of the format-specific functions would be very bad.

    ::

        struct NCGR *ncgr = nitro_read(fp, 0);
        if (ncgr == NULL) { goto error; }
        if (nitro_get_magic(ncgr) != NCGR_MAGIC) {
            u8 buf[MAGIC_BUF_SIZE];
            strmagic(ncgr, buf);
            fprintf(stderr, "Expected an NCGR, got %s", buf);
            goto error;
        }

.. c:function:: void nitro_free(void *chunk)

   Free a file opened with :func:`nitro_read` or :func:`narc_load_file`.

.. c:type:: magic_t

   An opaque type for the magic numbers at the beginning of each nitro file.

.. c:function:: inline magic_t nitro_get_magic(void *chunk)

   Returns the magic number from the nitro file.

.. c:function:: char *strmagic(magic_t magic, char *buf)

   Convert a :type:`magic_t` into its string representation. You must supply a buffer of at least ``MAGIC_BUF_SIZE`` bytes.

   Returns ``buf``.


NARC
----

.. c:type:: struct NARC

   Nitro ACHive. Files of this type contain other files.

.. c:function:: void *narc_load_file(struct NARC *, int index)

   Load a file from a NARC, given by ``index``.

   The first file is index 0. Program aborts if ``index`` is out of range.

   Returns a newly-allocated nitro object, or ``NULL`` in the case of an error.

.. c:function:: u32 narc_get_file_count(struct NARC *)

   Returns the count of files contained in the NARC.

.. c:function:: u32 narc_get_file_size(struct NARC *, int index)

   Returns the size of the file at the given index.

   Program aborts if ``index`` is out of range.

NCGR
----

.. c:type:: struct NCGR

   Nitro Character GRaphics. These files contain pixel data, usually indexed but occasionally true-color. In the former case, they must be combined with an NCLR to obtain a color image.

.. c:function:: int ncgr_get_dim(struct NCGR *, struct dim *dim)

   Set ``dim`` to the dimensions of the image.

   Returns an error code (but always succeeds).

.. c:function:: struct buffer *ncgr_get_pixels(struct NCGR *)

   Allocate and return a buffer containing the image data, unpacked and untiled. Data is formatted as an indexed raster image, one byte per pixel.

   Returns ``NULL`` in the case of error.

   .. note::
      Currently asserts that ``bit_depth == 3``, for some reason.

.. c:function:: struct buffer *ncgr_get_cell_pixels(struct NCGR *, u16 tile, struct dim cell_dim)

   Like :func:`ncgr_get_pixels`, but extracts only a portion of the image. Mainly used by :func:`ncer_draw_cell`.

   :param tile: The index of the starting tile.
   :param cell_dim: The dimensions of the subimage, in pixels.


.. c:function:: void ncgr_decrypt_dp(struct NCGR *)
                void ncgr_decrypt_pt(struct NCGR *)

   Decrypt the image data with either the Diamond/Pearl or Platinium algorithms.

NCLR
----

.. c:type:: struct NCLR

   Nitro CoLor Resource. These files contain palette data for NCGR_ files

.. c:function:: struct palette *nclr_get_palette(struct NCLR *, int index)

   Allocate and return a :type:`struct palette` containing the color data from the NCLR.

   The ``index`` argument must be 0.

   Returns ``NULL`` in case of error.

NCER
----

.. c:type:: struct NCER

   Nitro CEll Resource. Hard to explain.

.. c:function:: struct ncer_draw_cell(struct NCER *, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset)
                struct ncer_draw_cell_t(struct NCER *, int index, struct NCGR *ncgr, struct image *image, struct coords frame_offset, fx16 transform[4])

.. c:function:: struct ncer_draw_boxes(struct NCER *, int index, struct image *image, struct coords offset)

   Draw boxes around the cells in cellbank ``index``. The boxes are drawn with palette index 1.

   Used for debugging. 

NANR
----

.. c:type:: struct NANR

   Nitro Animation Resource.

.. c:var:: magic_t NANR_MAGIC

.. c:function:: int nanr_draw_frame(struct NANR *, int acell_index, int frame_index, struct NCER *ncer, struct NCGR *ncgr, struct image *image, struct coords frame_offset)

.. c:function:: int nanr_get_cell_count(struct NANR *)

.. c:function:: int nanr_get_frame_count(struct NANR *, int acell_index)

.. c:function:: int nanr_get_frame_at_tick(struct NANR *, int acell_index, u16 tick)

NMAR
----

.. c:type:: struct NMAR

   Nitro Mapped Animation Resource?

NMCR
----

.. c:type:: struct NMCR

   Nitro Mapped Cell Resource?


LZSS
----

LZSS is the compression algorithm used by the DS.
I have previously written about it on my blog: http://magikos.livejournal.com/7375.html.

On Linux, spriterip can transparently decompress files.
Windows is not supported because it does not support ``fmemopen(3)``.

There are a few things that could be done about this:

* The **boring** option is to rewrite lzss_decompress and the various fmt.read functions
  to read from memory (and write to memory).

* The **interesting** option is to write my own stream abstraction library.

* The **giving up** option is to rewrite spriterip in C++, or some other language with generic streams.

* The **cheating** option is to link against a different libc on Windows (instead of MSVCRT).
