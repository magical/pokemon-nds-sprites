
.. default-domain:: scm

Scheme API
==========

.. scm:function:: (make-image)
                  (make-image (x y))

    Construct a new image. If the first argument is supplied, memory will be allocated for an image of that size. Which isn't really useful at all.

    An image is, under the covers, of an array of pixel data and a palette, which can be set with :func:`image-set-pixels-from-ncgr` and :func:`image-set-palette-from-nclr`.


.. scm:function:: (load-narc path)

   Open a narc file from the filesystem.

   :raises: sys-error if the file is not found.
   :raises: misc-error if there was an error reading the file.

.. scm:function:: (narc-file-count narc)

   Return the number of files in the narc.

.. scm:function:: (narc-get-file-size narc i)

   Return the size of the file at the given index in a narc.

   Crashes with an assertion error if the index is out of bounds.

.. scm:function:: (narc-load-file narc index)
                  (narc-load-file narc index type)

   Load and return the file at the given index in a narc.

   The type parameter is a symbol giving the expected file (for example, ``'NCGR``. If provided
   If type is provided, assert that the file is the correct type.

   :returns: a nitro smob.
   :raises: ``narc-error`` if the type is provided and does not match.
   :raises: ``narc-error`` if the file type is not recognized by ripscript or if the file failed to load.

.. scm:function:: (get-magic nitro)

   Return a symbol representing the type of the given nitro file.

.. scm:function:: (ncgr-decrypt-pt ncgr)

   Decrypts the image data of an NCGR file with the algorithm used in Platinum.

.. scm:function:: (ncgr-decrypt-dp ncgr)

   Decrypts the image data of an NCGR file with the algorithm used in Diamond and Pearl.
