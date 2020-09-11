@doc Generic Transform takes care of transforming high-level data models to low-level data
models.  The aim of the {@link ec_genet} and {@link ec_genet_server} modules is to enable
a mapping writer to deal with simple cases as easily as possible, as well as to help with
more complex ones.

This is a brief overview of a mapping operation:

 1. the operator performs an action such as `commit`, `show running-config`
    etc. that addresses data in the high-level data model; ConfD invokes
    appropriate DP-API function in {@link ec_genet_server}
 2. based on the path prefix, appropriate mapping module and the registered
    function (such as {@link ec_map_bgp:get_mappings/1}) is looked up, invoked
    to get a mapping, `inherit` is processed
 3. if the mapping contains the right *override* callback (such as the `exists`
    field in case of existence check), it is invoked and the result returned to
    ConfD
 4. high-level path is rewritten to low-level path using callbacks like
    `fdnkeys` (if defined)
 5. the operation on the low-level path is performed (a leaf value set or
    retrieved, list instance found, etc.) and the result returned to ConfD.

== DP-API callbacks ==

The DP-API functions mentioned above are the only interface for ConfD to query
about the status of high-level configuration, or to notify about changes in the
high-level configuration. Some of the DP-API functions are optional; the
`ec_genet` framework implements the following ones (see the ConfD
documentation, `confd_lib_dp` manpage for more details on DP-API functions,
especially the documentation for `confd_register_data_cb`):

 * **`get_elem`**

    This callback function needs to return the value of a specific
    leaf. Invoked by ConfD during *show* and also during *set*
    operations to get a leaf value (or the `not_found` response), or on a list
    key for a particular list instance to verify whether the instance exists.

 * **`get_next`**

    This callback makes it possible for ConfD to traverse a set of list
    entries. Invoked during *show* operations on a list to retrieve -
    one by one - list instances (i.e. key values).

 * **`set_elem`**

    This callback writes the value of a leaf. Invoked during commit.

 * **`create`**

    Invoked by ConfD during commit to create a new list entry, a presence
    container, or a leaf of type empty.

 * **`remove`**

    This callback is used to remove an existing list entry, a presence
    container, or an optional leaf and all its sub nodes (if any) during
    commit. Its DP-API override counterpart is named as `delete`.

 * **`exists_optional`**

    Used by ConfD to detect existence of presence container or leafs of type
    empty during *show* and *set* operations. Its DP-API override
    counterpart is named as `exists`.

 * **`get_case`**

    Used by ConfD on a choice during *show* and *set* operations
    to query what of the choice cases exists.

 * **`set_case`**

    This callback will be invoked by ConfD during commit to set a case of a
    choice.

For each of the DP-API functions we can define a *override* (see below for more
on overrides); just note that a DP-API function and its *override* are not the
same things.


== The mappings record ==

The central part in guiding the genet code is the type {@link
ec_genet:mappings()}. Below is a complete list of fields that can be used; but
in typical cases only few of them need to be defined - in the simplest cases
only `path`, or `inherit` and `relpath`.

In many cases, the way how given field is used (if at all) is given by what
DP-API function has been invoked by ConfD. This also means that for mappings
attached to a particular node some fields would never be used - for instance,
defining `fupkeys` for a mapping attached to a leaf does not make sense.

Note that if for given DP-API invocation the mappings record contains the
corresponding *override*, other mappings fields are ignored.

**`path`**`: ikeypath()`

:  The destination low-level path. Can contain instantiated keys based on keys
   in the high-level path, or empty tuples (`{}`) that will be instantiated
   later using `fdnkeys`. See below for more on keys.

: Used: all DP-API invocations except when `inherit` and `relpath` is set.

**`inherit`**`: ikeypath()`

:  This fields indicates that the mapping should be derived from another
   mapping. The value of this field is a high-level path that will be used when
   looking up the derived-from mapping; the path is typically in the form
   `tl(HLPath)` (where `HLPath` is the high-level path being currently
   processed). Any fields defined in this mapping will override fields in the
   derived-from mapping.

:  Used: all DP-API invocations.

**`relpath`**`: ikeypath()`

:  A relative low-level path. Used with `inherit`; if the derived-from mapping
   defines `path` (directly or again through `inherit`), the field `path` for
   this mapping is set to concatenation of `relpath` and the inherited `path`.

:  Used: all DP-API invocations if `inherit` is set.

**`nested`**`: [mappings()]`

:  List of mappings that will be all sequentially applied after all path
   mapping functions have been applied. The top-level mapping does not need to
   contain the low-level path, in which case the nested mappings receive the
   unmodified high-level path.

:  Used: all DP-API invocations.

**`extra`**`: any()`

:  Any auxiliary data (e.g. key values) that will be needed in subsequent
   mapping functions (such as `fdnkeys`, `fdnval` etc.); the value of `extra`
   is passed unchanged as the `Extra` argument.

:  Used: all DP-API invocations.

**`fexists`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Extra :: any()) -> not_found | any()`

:  Guard callback invoked before any processing starts; if the callback returns
   `{ok, not_found}`, no further processing is performed and appropriate value
   is returned.

:  Used: `get_elem`, `exists`, `get_next`, `get_case`, and `delete` DP-API
   invocations.

**`fdnpath`**`: (Tctx :: confd_trans_ctx(), KeyedLLPath :: ikeypath(), Extra :: any()) -> ikeypath()`

:  Callback invoked with low-level path. It is supposed to be used if the
   built-in path processing mechanisms are not enough; receives preprocessed
   low-level path and should return the final low-level path.

:  Used: all DP-API invocations.

**`fopmap`**`: (Tctx :: confd_trans_ctx(), Op :: atom(), Path :: ikeypath(), Arg :: any(), Mappings :: mappings()) ->
    {confd_trans_ctx(), atom(), ikeypath(), any(), mappings()}`

:  Callback invoked after path has been transformed, but before the final
   low-level operation is performed. Can be used e.g. to change the destination
   operation.

:  Used: all DP-API invocations, except when the mapping contains `nested`.

**`fupkeys`**`: (Tctx :: confd_trans_ctx(), Op :: atom(), Keys :: key(), Extra :: any()) -> key()`

:  Transformation from low-level key values to high-level key values.

:  Used: only for `get_next` invocation on the result returned from the
   low-level configuration before the value is returned to ConfD.

**`fdnkeys`**`: (Tctx :: confd_trans_ctx(), KeySet :: [key()], Extra :: any()) -> [key()]`

:  Transformation from high-level key values to low-level key values. For
   simple value mapping such callback is not needed and the key value can be
   written directly into the low-level path (with the right mapping function);
   but if e.g. full access to the transaction is needed while transforming the
   value, `fdnkeys` callback is the way to go.

:  Used: for all DP-API invocations.

**`fupval`**`: (Tctx :: confd_trans_ctx(), Op :: atom(), RawVal :: value(), Extra :: any()) -> value()`

:  Callback to transform a leaf value from low-level to high-level data model.

:  Used for `get_elem`, `get_case`, or `exists` invocations.

**`fdnval`**`: (Tctx :: confd_trans_ctx(), Op :: atom(), RawVal :: value(), Extra :: any()) -> value()`

:  This is invoked to transform a high-level value to its low-level
   representation.

:  Used: for `set_elem` (the argument is a HL value), `set_case` (the argument
   is a choices/case name pair), and `move_after` (empty tuple - "move to the
   first position" - or a key set).


== DP-API overrides ==

In case the genet processing is too restrictive, it is possible to skip most of
it and use one of the following *overrides*; they correspond to their DP-API
counterparts, and given override can be invoked if and only if the
corresponding called DP-API function has been invoked for given element; the
only exception is that `get_elem` override is invoked if `exists_optional`
DP-API has been invoked and `exists` override not defined.

Again, this means that some combinations of node and an override for its
mapping are meaningless, such as `get_elem` override in a mapping for a list
will never be invoked.

 * **`get_elem`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Extra :: any()) ->
   value() | not_found | err()`
 * **`exists`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Extra :: any()) ->
   boolean() | err()`
 * **`create`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Extra :: any()) ->
   ok | err()`
 * **`delete`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Extra :: any()) ->
   ok | err()`
 * **`set_elem`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), Val :: value(), Extra :: any()) ->
   ok | err()`
 * **`get_next`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(), C :: maapi_cursor(), Extra :: any()) ->
   {false, undefined} | {ok, key(), maapi_cursor()} | err()`
 * **`get_case`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(),
                     Choice :: qtag() | [qtag()], Extra :: any()) ->
   qtag() | err()`
 * **`set_case`**`: (Tctx :: confd_trans_ctx(), HLPath :: ikeypath(),
                     Choice :: qtag() | [qtag()], qtag(), Extra :: any()) ->
   ok | err()`
