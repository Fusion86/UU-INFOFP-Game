module Weapons where

import Model

weaponShootCooldown :: WeaponType -> Float
weaponShootCooldown AssaultRifle = 0.1
weaponShootCooldown PeaShooter = 0.01
weaponShootCooldown SniperRifle = 0.8
weaponShootCooldown RocketLauncher = 2

weaponDamage :: WeaponType -> Float
weaponDamage AssaultRifle = 34
weaponDamage PeaShooter = 3.4
weaponDamage SniperRifle = 114
weaponDamage RocketLauncher = 200

weaponTravelSpeed :: WeaponType -> Float
weaponTravelSpeed AssaultRifle = 10
weaponTravelSpeed PeaShooter = 15
weaponTravelSpeed SniperRifle = 35
weaponTravelSpeed RocketLauncher = 5
