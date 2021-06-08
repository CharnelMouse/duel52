A program for playing Duel 52, to practise domain modelling for card games.

Work in progress: UI in place, basic play rules implemented. Need to implement card abilities.

- Basic actions: play card, activate, attack, make pair. Cards ready at the end of the turn.
- Players only see known information (except who's peeked at face-down cards).
- Bases become activatable when the draw pile empties.
- After the draw pile runs out, lanes are counted as won once only one player has cards there.
- A won lane stays won if it gets emptied by Move powers.
- A won lane can become contested again, or instantly won by another player if an action ends with another player's card present.
- Won lane counts are only used to check for victory once the player hands are all empty, since lanes cannot then be un-won.
  - Strictly speaking, the game ends when no one can take an action, but checking when hands are empty speeds things up.
- Card abilities remaining to implement:
  - View
  - Trap
  - Foresight
  - Flip
  - Freeze
  - Heal
  - Retaliate
  - Nimble
  - TwinStrike
  - Taunt
  - Vampiric (replaces Trap and Foresight in solo mode)
  - Move
  - Empower
  - Action

Future extensions:

- Variants with 1, 3, or 4 players
- Expansion: Heroes, Bannermen, Forts
