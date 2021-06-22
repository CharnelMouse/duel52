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
- Card ability implementation progress:
  - View [to implement]
  - Trap [implemented]
  - Foresight [to implement]
  - Flip [to implement]
  - Freeze [to implement]
  - Heal [implemented]
  - Retaliate [to implement]
  - Nimble
    - Immune to Freeze [to implement]
    - Immune to Retaliate [to implement]
    - Can't be secondary target for Twinstrike [to implement]
    - Extra damage to Taunt [implemented]
  - TwinStrike [to implement]
  - Taunt
    - 3 maximum health instead of 2 [implemented]
    - Must be primary target, non-Taunt allies can't be secondary target for Twinstrike [to implement]
  - Vampiric (replaces Trap and Foresight in solo mode) [to implement]
  - Move [to implement]
  - Empower [to implement]
  - Action [to implement]

Future extensions:

- Variants with 1, 3, or 4 players
- Expansion: Heroes, Bannermen, Forts
