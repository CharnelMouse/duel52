module PowerMaps
open Domain

let basePower: PowerMap = function
    | Two ->
        {
            Name = "View"
            OnActivation = [Draw 1u; Discard 1u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Three ->
        {
            Name = "Trap"
            OnActivation = []
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = [FullyHealSelf; ActivateSelf]
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Four ->
        {
            Name = "Foresight"
            OnActivation = [ViewInactive 1u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Five ->
        {
            Name = "Flip"
            OnActivation = [ActivateAlliesInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Six ->
        {
            Name = "Freeze"
            OnActivation = [FreezeEnemiesInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Seven ->
        {
            Name = "Heal"
            OnActivation = [HealAllAllies 2u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Eight ->
        {
            Name = "Retaliate"
            OnActivation = []
            OnAttack = []
            OnDamaged = [ReturnDamage]
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Nine ->
        {
            Name = "Nimble"
            OnActivation = []
            OnAttack = [ExtraDamageAgainstExtraMaxHealth 1u]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = [FreezeEnemiesInLane; ReturnDamage; DamageExtraTarget]
            WhileActive = []
        }
    | Ten ->
        {
            Name = "Twinstrike"
            OnActivation = []
            OnAttack = [DamageExtraTarget]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Jack ->
        {
            Name = "Taunt"
            OnActivation = []
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = [MaxHealthIncrease 1u; ProtectsNonTauntAlliesInLane]
        }
    | Queen ->
        {
            Name = "Move"
            OnActivation = [MayMoveAllyToOwnLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | King ->
        {
            Name = "Empower"
            OnActivation = [ReactivateNonEmpowerActivationPowersInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Ace ->
        {
            Name = "Action"
            OnActivation = [ExtraActions 1u; ChangeMaxAttacksThisTurn 2u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }

let soloPower: PowerMap = function
    | Two ->
        {
            Name = "View"
            OnActivation = [Draw 1u; Discard 1u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Three ->
        {
            Name = "Vampiric"
            OnActivation = []
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = [HealSelf 1u]
            Ignores = []
            WhileActive = []
        }
    | Four ->
        {
            Name = "Vampiric"
            OnActivation = []
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = [HealSelf 1u]
            Ignores = []
            WhileActive = []
        }
    | Five ->
        {
            Name = "Flip"
            OnActivation = [ActivateAlliesInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Six ->
        {
            Name = "Freeze"
            OnActivation = [FreezeEnemiesInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Seven ->
        {
            Name = "Heal"
            OnActivation = [HealAllAllies 2u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Eight ->
        {
            Name = "Retaliate"
            OnActivation = []
            OnAttack = []
            OnDamaged = [ReturnDamage]
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Nine ->
        {
            Name = "Nimble"
            OnActivation = []
            OnAttack = [ExtraDamageAgainstExtraMaxHealth 1u]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = [FreezeEnemiesInLane; ReturnDamage; DamageExtraTarget]
            WhileActive = []
        }
    | Ten ->
        {
            Name = "Twinstrike"
            OnActivation = []
            OnAttack = [DamageExtraTarget]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Jack ->
        {
            Name = "Taunt"
            OnActivation = []
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = [MaxHealthIncrease 1u; ProtectsNonTauntAlliesInLane]
        }
    | Queen ->
        {
            Name = "Move"
            OnActivation = [MayMoveAllyToOwnLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | King ->
        {
            Name = "Empower"
            OnActivation = [ReactivateNonEmpowerActivationPowersInLane]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
    | Ace ->
        {
            Name = "Action"
            OnActivation = [ExtraActions 1u; ChangeMaxAttacksThisTurn 2u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
