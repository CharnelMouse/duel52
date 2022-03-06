module PowerMaps
open Domain

let basePower: PowerMap = function
    | Two ->
        {
            Name = PowerName "View"
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
            Name = PowerName "Trap"
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
            Name = PowerName "Foresight"
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
            Name = PowerName "Flip"
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
            Name = PowerName "Freeze"
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
            Name = PowerName "Heal"
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
            Name = PowerName "Retaliate"
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
            Name = PowerName "Nimble"
            OnActivation = []
            OnAttack = [ExtraDamageAgainstExtraMaxHealth 1u]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = [InstantNonTargetAbility FreezeEnemiesInLane; DefendAbility ReturnDamage; AttackAbility DamageExtraTarget]
            WhileActive = []
        }
    | Ten ->
        {
            Name = PowerName "Twinstrike"
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
            Name = PowerName "Taunt"
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
            Name = PowerName "Move"
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
            Name = PowerName "Empower"
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
            Name = PowerName "Action"
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
            Name = PowerName "View"
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
            Name = PowerName "Vampiric"
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
            Name = PowerName "Vampiric"
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
            Name = PowerName "Flip"
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
            Name = PowerName "Freeze"
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
            Name = PowerName "Heal"
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
            Name = PowerName "Retaliate"
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
            Name = PowerName "Nimble"
            OnActivation = []
            OnAttack = [ExtraDamageAgainstExtraMaxHealth 1u]
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = [InstantNonTargetAbility FreezeEnemiesInLane; DefendAbility ReturnDamage; AttackAbility DamageExtraTarget]
            WhileActive = []
        }
    | Ten ->
        {
            Name = PowerName "Twinstrike"
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
            Name = PowerName "Taunt"
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
            Name = PowerName "Move"
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
            Name = PowerName "Empower"
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
            Name = PowerName "Action"
            OnActivation = [ExtraActions 1u; ChangeMaxAttacksThisTurn 2u]
            OnAttack = []
            OnDamaged = []
            OnInactiveDying = []
            OnKill = []
            Ignores = []
            WhileActive = []
        }
