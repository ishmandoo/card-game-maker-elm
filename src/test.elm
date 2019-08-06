import Browser
import Html exposing (Html, text, pre, div, span)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder, map2, field, string, int, list)
import Http
import Dict exposing (Dict, get, empty, insert, update)
import Debug


type alias Card =
  { name : String
  , copies : Int
  , templateName: String
  , data : List String
  }

type alias Template = 
  { name : String
  , fields : List Field
  }

type alias Field =
  { templateName : String
  , x : Float
  , y : Float
  }

makeField : List String -> Result String Field
makeField data = 
  case data of
    [templateName, x, y] -> 
      let (xMaybe, yMaybe) = (String.toFloat x, String.toFloat y) in
        case (xMaybe, yMaybe) of
          (Just xFloat, Just yFloat) -> 
            Ok (Field templateName xFloat yFloat)
          _ -> 
            Err "Invalid coordinates"
    _ -> Err "Missing field for Field"

makeCard : List String -> Result String Card
makeCard list =
  case list of
    name::copies::templateName::data -> 
      let maybeCopies = (String.toInt copies) in
        case maybeCopies of
          Just intCopies -> Ok (Card name intCopies templateName data)
          Nothing -> Err "Non-integer copies"
    _ -> Err "Missing field for Card"

makeTemplates : List (Result String Field) -> Dict String Template -> Dict String Template
makeTemplates fields templates = 
  case fields of
    (Ok field::xs) -> 
      let templateMaybe = (get field.templateName templates) in
        case templateMaybe of
          Just template -> makeTemplates xs (insert field.templateName (addField template field) templates)
          Nothing -> makeTemplates xs (insert field.templateName (Template field.templateName [field]) templates)
    (Err _::xs) -> makeTemplates xs templates
    [] -> templates

addField : Template -> Field -> Template
addField template field = Template template.name (template.fields ++ [field])

attributeListDecoder : Decoder (List (List String))
attributeListDecoder =
  field "values" (list (list string))

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | LoadingTemplates
  | LoadingCards (Dict String Template)
  | Success (List (Result String Card)) (Dict String Template)


init : () -> (Model, Cmd Msg)
init _ =
  ( LoadingTemplates
  , Http.get
    { url = "https://sheets.googleapis.com/v4/spreadsheets/1XsODEbYWDBwPo0QclvZ0oeuW9B9ROPfDkJjulIMnMEw/values/templates!2:100?key=AIzaSyDpq2gDAA0Uxc_rzdK26cb9AdcbUUJe7rE"
    , expect = Http.expectJson GotList attributeListDecoder
    }
  )



-- UPDATE


type Msg
  = GotList (Result Http.Error (List (List String)))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
      case msg of
        GotList result ->
          case result of
            Ok fullText ->
              case model of
                LoadingTemplates -> 
                  ( LoadingCards (makeTemplates (List.map makeField fullText) empty)
                    , Http.get
                      { url = "https://sheets.googleapis.com/v4/spreadsheets/1XsODEbYWDBwPo0QclvZ0oeuW9B9ROPfDkJjulIMnMEw/values/cards!2:100?key=AIzaSyDpq2gDAA0Uxc_rzdK26cb9AdcbUUJe7rE"
                      , expect = Http.expectJson GotList attributeListDecoder
                      }
                  )
                LoadingCards templates -> 
                  (Success (List.map (makeCard) fullText) templates
                    , Cmd.none
                  )
                Failure ->
                  (Failure, Cmd.none)
                Success cards templates -> 
                  (Success cards templates, Cmd.none)
            Err _ ->
              (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

formatCards : List (Result String Card) -> Dict String Template -> List (Html Msg)
formatCards cards templates =
  case cards of
    ((Ok card)::xs) ->           
      let templateMaybe = get card.templateName templates in
        case templateMaybe of
          Just template ->
            if card.copies > 0 then
              (formatCard card template)::(formatCards (Ok (Card card.name (card.copies-1) card.templateName card.data)::xs) templates)
            else
              formatCards xs templates
          Nothing ->
            formatCards xs templates
    ((Err _)::xs) ->  formatCards xs templates
    [] -> []

formatCard : Card -> Template -> Html Msg
formatCard card template = div [style "width" "2.5in", style "height" "3.5in", style "border" "1px solid black", style "float" "left", style "position" "relative"] ((text (card.name))::(List.map2 formatField template.fields card.data))

formatField : Field -> String -> Html Msg
formatField field data =
  div [style "left" ((String.fromFloat field.x)++"in"), style "top" ((String.fromFloat field.y)++"in"), style "position" "absolute"] [text data]


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    LoadingTemplates ->
      text "Loading Templates..."

    LoadingCards templates ->
      text "Loading Cards..."

    Success cards templates ->
      div [style "width" "11in", style "height" "8.5in"] (formatCards cards templates)