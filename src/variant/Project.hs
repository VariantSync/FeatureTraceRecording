module Project where

import Variant
import Configuration

type Project g a = Project g a {
    variants :: [Variant g a] -- a.k.a. clones, branches, forks
    -- , featuremodel :: PropositionalFormula a
}

type TargetVariantSelector g a = RecordedEdit g a -> Variant g a -> Variant g a -> Bool

commit :: Eq a => Project g a -> TargetVariantSelector g a -> Variant g a -> RecordedEdit g a -> Project g a
commit project isTarget variant redit
    | not $ elem variant (variants project) = error "The given variant is not part of the given project!"
    | otherwise = project {
        variants = (\v ->
                if isTarget redit variant v
                then evolve [redit] v -- Do we have to mutate the edit? For instance when edit is a move, do we have to delete the artefact from variants not satisfying the new trace?
                else v) <$> (variants projects)
      }

nosyncTargetVariantSelector :: TargetVariantSelector g a
nosyncTargetVariantSelector _ initialvariant variant = initialvariant == variant

contextBasedTargetVariantSelector :: TargetVariantSelector g a
nosyncTargetVariantSelector (_, context) _ variant = satisfies (config variant) context

-- editBasedTargetVariantSelector :: TargetVariantSelector g a
-- editBasedTargetVariantSelector (edit, context) _ variant =
--     case edittypeof edit of
--         Insert -> ...