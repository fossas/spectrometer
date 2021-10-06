{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}

module Data.Tagged (
  Tagged,
  enforceTag,
  unTag,
  applyTag,
  ConstTag (..),
  readTag,
) where

import GHC.Generics (Generic)

-- INTERNAL NOTES
-- We want to inherit as much as we can from the inner type, deriving will exclude things that don't fit.
-- We set a non-default type role, to prevent 'coerce' from changing these strictly type values into others silently.

-- | The Tagged newtype allows an arbitrary tag type to accompany any other type.
-- @ty@ is representational, which is the default type role for that type parameter.
--   This means that @ty -> ty'@ can be done by coerce is the runtime representation is the same.
-- @tag@ is nominal, though the default role is phantom.  With phantom type roles, GHC allows
--   coerce for any reason, since there is no type.  However the type safety of the 'Tagged' type
--   comes from not allowing coerce to do any magic, so we use nominal, which means that coerce
--   only allows exact type matches, regardless of representation.  Essentially, @coerce = id@.
--
-- In practice:
-- @
--   data SomeTag
--   data SomeOtherTag
--
--   doCoerce :: Tagged a SomeTag -> Tagged a SomeTag
--   doCoerce = coerce
--
--   dontCoerce :: Tagged ty SomeTag -> Tagged ty SomeOtherTag
--   dontCoerce = coerce  -- Compile error: SomeTag is not SomeOtherTag
--
--   -- This is a representation of the pattern that a Tagged item avoids.
--   myInt :: Int
--   myInt = unTag @SomeOtherTag $ coerce $ applyTag @SomeTag (3 :: Int)
-- @
newtype Tagged ty tag = Tagged {inner :: ty}
  deriving
    ( Eq
    , Ord
    , Show
    , Semigroup
    , Monoid
    , Generic
    , Functor
    , Foldable
    , Traversable
    )

type role Tagged representational nominal

-- | Enable strict type checking of tags with @-XTypeApplications@.
-- > enforceTag @MyTag (x :: Tagged ty MyTag) = id
enforceTag :: forall tag ty. Tagged ty tag -> Tagged ty tag
enforceTag = id

-- | Same as 'enforceTag', but extracts the inner @ty@.
unTag :: forall tag ty. Tagged ty tag -> ty
unTag = inner

-- | @applyTag ty@ tags a @ty@ with the given tag.  With type applications, this can be enforced more easily.
applyTag :: forall tag ty. ty -> Tagged ty tag
applyTag = Tagged

-- | 'ConstTag' allows the @tag@ type of a 'Tagged' to provide a constant value, accessible through 'readTag'.
-- When defining instances, you usually want to use @const x@, where x is the value you really want to embed.
-- This is because GHC forces us to provide some information about which tag we're using, and most tag types
-- are empty data types (types without constructors, and therefore values).  We cannot pass in a value of that
-- type, and therefore must use a 'Proxy' to carry the type information of the tag.
class ConstTag tag b | tag -> b where
  constValue :: b

readTag :: forall tag ty b. ConstTag tag b => Tagged ty tag -> b
readTag _ = constValue @tag
