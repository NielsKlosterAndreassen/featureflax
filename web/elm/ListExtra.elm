module ListExtra exposing (..)

import Set exposing (Set)

unique : List comparable -> List comparable
unique list =
  uniqueHelp Set.empty list


uniqueHelp : Set comparable -> List comparable -> List comparable
uniqueHelp existing remaining =
  case remaining of
    [] ->
      []

    first :: rest ->
      if Set.member first existing then
        uniqueHelp existing rest
      else
        first :: uniqueHelp (Set.insert first existing) rest

find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first::rest ->
            if predicate first then
                Just first
            else
                find predicate rest
