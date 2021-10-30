module Weapons where

import Model

weaponShootCooldown :: WeaponType -> Float
weaponShootCooldown AssaultRifle = 0.1
weaponShootCooldown PeaShooter = 0.07
weaponShootCooldown SniperRifle = 0.8
weaponShootCooldown RocketLauncher = 1

weaponDamage :: WeaponType -> Float
weaponDamage AssaultRifle = 34
weaponDamage PeaShooter = 3.4
weaponDamage SniperRifle = 114
weaponDamage RocketLauncher = 200

weaponTravelSpeed :: WeaponType -> Float
weaponTravelSpeed AssaultRifle = 8 * 715 / 6 -- ak47
weaponTravelSpeed PeaShooter = 8 * 400 / 6 -- mp9
weaponTravelSpeed SniperRifle = 8 * 936 / 4 -- l98 lapua / awp
weaponTravelSpeed RocketLauncher = 8 * 300 / 6 -- rpg-7 flight
