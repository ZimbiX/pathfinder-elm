reset() {
  curl -sSf 'www.zimbico.net/pathfinder-elm-backend/db-setup.php?reset'
  i=0
}

game_id() {
  sed -n -E 's/^.* gameId = "(.*)".*$/\1/p' src/Main.elm
}

move() {
  local direction="$1"
  curl -sSf -i \
    www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php \
    -d id="$(game_id)" \
    -d event='{"name": "'"$direction"'", "data": {}}' \
    -d version=$((++i))
}

all() {
  curl -sS "http://www.zimbico.net/pathfinder-elm-backend/pathfinder-elm-backend.php?id=$(game_id)&after=0&all" -v | jq .
}

alias w='move MoveUp'
alias a='move MoveLeft'
alias s='move MoveDown'
alias d='move MoveRight'
alias r='reset'
