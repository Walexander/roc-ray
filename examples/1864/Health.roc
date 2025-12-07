module [isAlive, update, range, new, takeHit, health, make, Health]
Health := { now : U32, base : U32 }
make : U32 -> Health
make = \base -> @Health { base: base, now: base }

health : Health -> Frac *
health = \@Health { now, base } ->
    Num.to_frac now
    |> Num.max 0
    |> Num.div (Num.to_frac base)

takeHit : Health, U32 -> Health
takeHit = \@Health { now, base }, damage ->
    next = if damage >= now then 0 else (now - damage)
    @Health { base, now: next }

isAlive = \@Health { now } -> now > 0

expect
    testHealth = make 100
    actual = health testHealth
    Num.is_approx_eq actual 1.0 {}

CombatStats : {
    type : [Infantry, Artillery, Cavalry],
    entity : I8,
    readiness : [Targeting I8, Defending, Attacking I8, Mustering],
    lastFired : U32,
    rate : U32,
    range : U8,
    damage : U32,
}
Combatant := CombatStats
new = \stats -> @Combatant stats

range : Combatant -> U8
range = \@Combatant stats -> stats.range

# fireAway = \@Combatant stats, enemies, frame ->
#     Hit { victimId: 3, damage: 25, frame }

testCombatant = Health.new {
    type: Infantry,
    entity: 1,
    readiness: Defending,
    lastFired: 0,
    rate: 200,
    range: 1,
    damage: 25,
}

expect
    actual = range testCombatant
    actual == 1

update = |world|
    {
        world&
        units: List.map world.units | u |
            when u.health is
                Dead _ -> u
                Living h if isAlive h -> u
                Living _ -> {
                    u&
                    health: Dead 0
                }
    }
