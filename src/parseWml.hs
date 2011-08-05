-- file: parseWml.hs

module ParseWml
(
  parseWml
) 
    where

import Data.List
import Text.Parsec.Prim (unexpected)

import ApplicativeParsec

-- default node type including top level
data Node = Node
    {
      nName :: String
    , nBody :: NodeBody
    }
        deriving (Eq, Show)

emptyNode = NNode $ N $ Node "" []

-- merge nodes 
data MergeNode = MergeNode
    {
      mName :: String
    , mBody :: NodeBody
    }
        deriving (Eq, Show)

type NodeBody = [NodeItem]

data WmlConfig = N Node
               | M MergeNode
        deriving (Eq, Show)

data NodeItem = NM ()
              | NH ()
              | NAtt Attribute
              | NMatt Mattribute 
              | NNode WmlConfig
        deriving (Eq, Show)

parseWml s = parse topLevel "" s

-- topLevel = 
topLevel = manyTill (spaces *> topLevelItem) eof

topLevelItem =
        marker *> sourceOrDomain *> spaces *> return emptyNode  -- deal with \376
    <|> hash *> domain *> spaces *> return emptyNode            -- deal with #
    <|> topLevelNode

sourceOrDomain =
        char 'l' *> sourceInfo
    <|> domain

sourceInfo = do
    many (noneOf "\n")
    anyChar

domain = do
    many (noneOf "\n")
    anyChar

-- Main node parser
parseNode :: (a -> WmlConfig) -> (String -> NodeBody -> a) -> CharParser () NodeItem
parseNode c1 c2 = do
    start <- tagName
    body <- nodeBody
    end <- tagName
    mkNode c1 c2 start body end

tagName = manyTill namechars rb <* spaces

mkNode c1 c2 s b e
    | s == e = return $ NNode $ c1 $ c2 s b
    | otherwise = unexpected (": end tag '" ++ e ++ "' does not match '" ++ s ++ "'")

-- 'constructors' for different node types
node = parseNode N Node

mergeNode = parseNode M MergeNode

-- Top Level Node parser 
topLevelNode = do
    lb
    internalNode

-- parsing node bodies. NB no 'try's.
nodeBody = 
        lb *> maybeEndBody
    <|> marker *> sourceOrDomain *> spaces *> nodeBody
    <|> hash *> domain *> spaces *> nodeBody
    <|> attributeThenNodeBody

-- maybeEndBody
-- we just hit [, so its either an end tag (NB don't consume the tag name)  
-- or a node followed by...
maybeEndBody =
        char '/' *> (return [])
    <|> do n <- internalNode
           spaces
           r <- nodeBody
           return (n : r)

-- attributeThenNodeBody
-- we didn't see [, so must be an attribute followed by ...
attributeThenNodeBody = do
    a <- attribute
    spaces
    b <- nodeBody
    return (a : b)

internalNode =
        char '+' *> mergeNode
    <|> node

-- Attributes
data Attribute = Attribute
    {
      aKey  :: String
    , aValue :: AttributeValue
    }
        deriving (Eq, Show)

data Mattribute = Mattribute
    {
      mKeys  :: [String]
    , mValues :: [AttributeValue]
    }
        deriving (Eq, Show)

attribute = do
    spaces
    key <- attName
    keys <- maybeKeys
    finishAtrribute key keys

attName = many namechars <* spaces

maybeKeys = do
        char ','
        spaces
        key <- attName
        keys <- maybeKeys
        return (key : keys)
    <|> return [] 

finishAtrribute key [] = do
    char '='
    spaces
    value <- attributeValue
    return $ NAtt $ Attribute key value
finishAtrribute key keys = do
    char '='
    spaces
    value <- attributeValue
    values <- maybeValues
    return $ NMatt $ Mattribute (key:keys) (value:values)

maybeValues = do 
        char ','
        spaces
        value <- attributeValue
        values <- maybeValues
        return (value : values)
    <|> return [] 

data AttributeValue = Variable String
                    | Formula String
                    | String [StringValue]
        deriving (Eq, Show)

attributeValue =
        marker *> sourceOrDomain *> spaces *> attributeValue
    <|> Variable <$> wmlVariableValue
    <|> String <$> leadingUnderscore
    <|> Formula <$> formulaValue
    <|> String <$> stringValue
    <|> String <$> defaultAttValue

wmlVariableValue = char '$' *> many namechars <* spaces

leadingUnderscore = do
    try(char '_' *> noneOf " \"\n[" >>= leadingUnderscore')

leadingUnderscore' c = do
    v <- many  (noneOf "\n[")
    return $ [QuotedString (c:v)]


formulaValue = try(string "\"$(") *> many (noneOf ")") <* anyChar

data StringValue = Translatable String
                 | QuotedString String
        deriving (Eq, Show)
 
stringValue = do 
    s <- quotedStringValue
    r <- stringValueRest
    return (s:r)

stringValueRest = 
        marker *> sourceOrDomain *> spaces *> stringValueRest
    <|> try(plus) *> stringValueRest'
    <|> return []

stringValueRest' = 
        marker *> sourceOrDomain *> spaces *> stringValueRest'
    <|> stringValue

quotedStringValue = 
        translatableString
    <|> quotedString

translatableString = do
    char '_' 
    spaces 
    s <- simpleQuotedString
    return $ Translatable s

quotedString = do
    s <- simpleQuotedString
    return $ QuotedString s

simpleQuotedString = do
    char '"'
    s <- manyTill (noneOf "\"") (char '"')
    r <- maybeQuotedString
    return $ s ++ r

maybeQuotedString =
        do { s <- simpleQuotedString; return ('"' : s)}
    <|> return ""

defaultAttValue = do
    v <- many  (noneOf "\n[")
    return $ [QuotedString v]

lb = char '['
rb = char ']'

namechars' = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
namechars = oneOf namechars' 

marker = char '\376'
hash = char '#'

plus = spaces *> char '+' <* spaces

t1 = "[tag]"
t2 = "key"
t4 = "\"value\"\n"
t5 = "\"va\"\"lue\"\n"
t6 = "\"va\"\"lue\" + \" of god\"\n"
t7 = " + \" of god\"\n "
t3 = "key=\"value\"\n[/tag] "

t10 = "[tag]\n     key=value\n[/tag]"
t11 = "[tag]\n     key=value\n[/ta]"
t12 = "[tag]\n     [tag1]\n     key1=value1\n[/tag1] \n [/tag]"
t13 = "[tag]\n     key=value\n   [xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t14= "[tag]\n     key=value\n   [tag1]\n     key1=value1\n[/tag] \n [/tag]"
t15= "[+xxx]\n     key1=value1\n[/xxx]\n"
t16= "[tag]\n     key=value\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t17= "[tag]\n     key=\"value\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t18= "[tag]\n     key=\"va\"\"lue\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t19= "[tag]\n     key=\"va\" + \"lue\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t20= "[tag]\n     key=_\"value\"\n   [+xxx]\n     key1=value1\n[/xxx] \n [/tag]"
t21= "[tag]\n     key=$value\n   [+xxx]\n     key1=_\"value1\"\n[/xxx] \n [/tag]"
t22= "[tag]\n     key=\"$(value)\"\n   [+xxx]\n     key1=_\"value1\"\n[/xxx] \n [/tag]"

w1 = "[multiplayer]\n\n    [side]\n[unit]\n        type=\"Elvish\n        name=Marksman\"\n        x=\"\"\n        y=2\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 119 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            \n            description=_ \"Slim of Stature, dexterous Hands\n- - Seven Stones and Eleven\nLeft his Foes so slim a Chance\n- - Seven Stones and Eleven\nMarksman known as Dragonbane\n'mongst the Statues here was slain\n- - Seven Stones - and the Elven\n(inscribed by Gauteamus)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 133 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Direwolf\n        name=Rider\"\n        x=_\"Blum Duk\"\n        y=10\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 133 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            description=_ \"Blum Duk was renowned among his goblin clan for having tamed one of the Dire Wolves of the mountains, and he had the courage to match. His leadership and skills alone were what kept the area's goblins alive despite human and elvish menaces. Rumor had it that his wolf had slain fifty men and a hundred Elves.\nThus, when he heard of the awful monster that was inhabiting the area, it was only natural that he ride out alone to face it. All the other goblins expected him to slay the creature easily and drag back its carcass to feast on.\nHe was in for quite a shock. As soon as he saw the Basilisk, Blum Duk cowered in fright. He tried to wheel his wolf around to run away, but it wouldn't move. As the creature stalked towards him, he screamed his last words: Good Gog, dog, are yer legs made of stone?!?\n(inscribed by Elvish Pillager)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 142 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Armageddon\n        name=Drake\"\n        x=_\"Rah\n        y=Ihn\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 142 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            description=_ \"Rah Ihn Mar, Champion of the Burning Hills, made his way to the Ruaskkolin Lake region in pursuit of the Elvish Hero Terowydlithrol, known among Elves as ''The Dragonbane.'' It was said that Terowydlithrol had killed a Dragon of Fire that threatened an Elvish village, and initiated a ''counterattack'' in which some 37 Drakes, camping in a nearby ravine and believed to be in allegiance with the Dragon, were slaughtered. Knowing that these Drakes were innocent, Rah Ihn Mar came to Ruaskkolin Lake to hunt down and slay Terowydlithrol, who had been seen entering the region but a day ago. Burning for vengeance, Rah Ihn Mar began the hunt.....\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 148 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Sea\n        name=Serpent\"\n        x=\"\"\n        y=22\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 148 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            facing=sw\n            description=_ \"He had lived there for ages upon ages, in the depths of the water, preying upon the various fish and frogs and merfolk that entered his domain. It was his lake, no other's, and though it was not large, he was its master. Then, the Basilisk came, and looked into the Serpent's eyes. And the Serpent had not died, but it was no longer alive. Thus the new lord of the lake had arrived.\n(inscribed by Turin)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 156 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Grand\n        name=Knight\"\n        x=\"\"\n        y=25\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 156 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            facing=sw\n            description=_ \"A brave hero of Wesnoth's Golden Age, this great rider and commander of men came to this evil place while on a raid against the orcs. Caught unaware by the foul creatures, many of his fellows were killed, but he fought valiantly, slaying many a foe. But alas! The foul Basilisk turned him to stone even as he slew the last orc. Now all that is left is a stark reminder of his once great bearing and strength.\n(inscribed by Fynmiir)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 164 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Elder\n        name=Wose\"\n        x=_\"Bramwythl\"\n        y=32\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 164 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            description=_ \"Bramwythl the Wose was always considered a slow, taciturn fellow, even by other Woses. More than any other Wose, he enjoyed simply standing alone in the sun, arms upraised, admiring the beauty of the empty sky. Once, even, he confided in a fellow that his greatest wish was to be able to bask forever in the sun's glory.\nHowever, not long after, the local wose community heard of the approach of a horrible monster, powerful enough to easily destroy the few woses that lived in the area. They quickly moved off (quickly for Woses, anyway), but Bramwythl was left behind - in their haste, no one had remembered to find him and tell him of the danger.\nHe was taken quite unawares by the Basilisk, and turned to stone before he even recognized the beast. His petrified form still stands there today, warmed by the sun's rays, under an open sky. His greatest wish has been granted.\n(inscribed by Elvish Pillager)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 173 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Iron\n        name=Mauler\"\n        x=_\"Talael\n        y=Ryndoc\"\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 173 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            facing=sw\n            description=_ \"General Talael Ryndoc came to this place to fight a duel with the Grand Marshal Aethec Corryn, but the two men never found one another. \"\n        [/unit]\n\n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 179 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Grand\n        name=Marshal\"\n        x=_\"Aethec\n        y=Corryn\"\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 179 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            facing=sw\n            description=_ \"The last words spoken by Grand Marshal Aethec Corryn: ''Talael Ryndoc! I am through searching for you! I shall make my way back to Haldric's Hall, and inform the court that you were too much of a coward to attend our duel! Your disgrace shall be more agonizing than the death I'd have given you!''\n\nPerhaps he should not have shouted quite so loudly.... \"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 188 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Naga\n        name=Myrmidon\"\n        x=_\"Rilhon\"\n        y=20\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 188 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            description=_ \"This brave warrior, known as Rilhon among the Naga, heard tell of a fabulous treasure that had been lost in these watery caves- a spear whose head was said to have been fashioned from the tooth of Chak'kso Ney'yks, an Elder Basilisk slain long ago by the Elvish hero, Eloralduil. According to the legend, the weapon rested somewhere on the bottom of this very lake (Ruaskkolin Lake, named for a well-known Sea Serpent who made it his home). Having prepared himself for battle with the Serpent, Rilhon met with two rather large surprises. The first was the petrified form of Ruaskkolin the Serpent. The second was the very-much-alive Chak'kso Ney'yks, who was, oddly enough, in possession of all of his teeth.\n(inscribed by Paterson)\"\n        [/unit]\n\n        \n        \376line 4 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg 195 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n[unit]\n        type=\"Mermaid\n        name=Siren\"\n        x=\"\"\n        y=17\n        unrenamable=yes\n        random_traits=no\n\n        [status]\n            petrified=on\n        [/status]\n\n        [modifications]\n            [trait]\n                id=remove_hp\n                [effect]\n                    apply_to=hitpoints\n                    increase_total=-100%\n                [/effect]\n                [effect]\n                    apply_to=movement\n                    set=0\n                [/effect]\n            [/trait]\n        [/modifications]\n    [/unit]\n\376line 195 multiplayer\\scenarios\\2p_Caves_of_the_Basilisk.cfg 15 multiplayer\\_main.cfg 8 core\\editor\\_main.cfg\n\376textdomain wesnoth-multiplayer\n\n        [+unit]\n            facing=sw\n            description=_ \"Through waves and rocky channels\nblue and white\nshe pulled and pushed the tides\nand taught the fishes how to speak to planets\nsilver green\nwith magic\nrunning from her spirit into skins of kelp and shells of snails\na twisting of her tail and hands\nshe sent her songs to run on pin-tipped legs about the sands\nand now\nand now the strangest pause\nfor years and years\nshe hasn't moved her eyes\n\n(inscribed by MJQ)\"\n        [/unit]\n    [/side]\n\n\n[/multiplayer]\n\n\n"

p = parseWml