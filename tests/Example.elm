module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (..)
import Test exposing (..)


update_quality_of : Item -> Item
update_quality_of item =
    case List.head (update_quality [ item ]) of
        Just f ->
            f

        _ ->
            Item "null" 0 0


suite : Test
suite =
    describe "guilded rose"
        [ describe "normal items"
            [ test "when quality is updated, an item's sell by decreases by 1"
                (\_ ->
                    let
                        shop_item =
                            update_quality_of (Item "some item" 10 9)
                    in
                    Expect.equal shop_item.sell_by 9
                )
            , test "when quality is updated, an item's quality decreases by 1" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "some item" 10 9)
                    in
                    Expect.equal shop_item.quality 8
            , test "when sell by date has passed, quality degrades by 2" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "some item" -1 9)
                    in
                    Expect.equal shop_item.quality 7
            , test "quality is never below 0 when sell by is not negative" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "some item" 2 0)
                    in
                    Expect.equal shop_item.quality 0
            , test "quality is never below 0 when sell by is negative" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "some item" -10 1)
                    in
                    Expect.equal shop_item.quality 0
            , test "quality is never above 50" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "aged brie" 10 50)
                    in
                    Expect.equal shop_item.quality 50
            ]
        , describe "aged brie"
            [ test "increases in quality after each day" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "aged brie" 10 1)
                    in
                    Expect.equal shop_item.quality 2
            ]
        , describe "sulfuras"
            [ test "never increases in quality" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "sulfuras" 10 80)
                    in
                    Expect.equal shop_item.quality 80
            , test "sell by never decreases" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "sulfuras" 10 80)
                    in
                    Expect.equal shop_item.sell_by 10
            ]
        , describe "Backstage passes"
            [ test "quality increases by 1 each day" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "backstage passes" 12 10)
                    in
                    Expect.equal shop_item.quality 11
            , test "quality increases by 2 when sell_by is less than 11" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "backstage passes" 10 10)
                    in
                    Expect.equal shop_item.quality 12
            , test "quality increases by 3 when sell_by is less than 6" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "backstage passes" 5 10)
                    in
                    Expect.equal shop_item.quality 13
            , test "quality is 0 when sell by is negative" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "backstage passes" -20 0)
                    in
                    Expect.equal shop_item.quality 0
            ]
        , describe "Conjured" 
        [
             test "degrades by two each day" <|
                \_ ->
                    let
                        shop_item =
                            update_quality_of (Item "conjured" 10 20)
                    in
                    Expect.equal shop_item.quality 18
        ]
     ]
