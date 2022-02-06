{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Card where

import qualified Data.Text                     as StrictText
import           Data.Text.Lazy                 ( Text
                                                , toStrict
                                                )
import           Data.UUID                      ( UUID
                                                , toText
                                                )
import           NeatInterpolation
import           Parser


data Entry = Entry
    { title     :: Text
    , uuid      :: UUID
    , content   :: Mark
    , entryType :: Type
    }
    deriving (Show, Eq)

data Type = Topic
          | Item
          deriving (Show, Eq)

generateTopicCard :: Entry -> StrictText.Text
generateTopicCard (Entry (toStrict -> title) uuid@(toText -> utx) content (StrictText.pack . show -> typ)) = [trimming|
Begin Element #0
Source=e:\@data\document\tool\sm18-lazy-package-1.2.0\sm18\systems\abc of supermemo 18
Parent=56
ParentTitle=TODO
Priority=4.97678
Begin ElementInfo #0
Title=${title}
Type=${typ}
Status=Memorized
FirstGrade=8
Ordinal=1507.000000
Repetitions=1
Lapses=0
Interval=1
LastRepetition=04.02.22
AFactor=1.200
UFactor=1.000
ForgettingIndex=10
Reference=
SourceArticle=0
End ElementInfo #0
ElementColor=-16777211
AutoPlay=1
BackgroundImage=
BackgroundFile=
BackgroundStyle=Tile
Scaled=1
ReadPointComponent=0
ReadPointStart=0
ReadPointLength=0
ReadPointScrollTop=0
ComponentNo=1
Begin Component #1
Type=HTML
Cors=(100,200,9700,9300)
DisplayAt=255
Hyperlink=0
HTMName=aabb
HTMFile=e:\@data\document\tool\sm18-lazy-package-1.2.0\sm18\systems\abc of supermemo 18\elements\generated\${utx}.HTM
TestElement=0
ReadOnly=0
FullHTML=1
Style=0
End Component #1
Begin RepHist #0
End RepHist #0
End Element #0

    |]
