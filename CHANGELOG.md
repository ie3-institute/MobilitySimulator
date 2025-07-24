# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added 
- Allow parsing of PSDM `EvInputs` [#112](https://github.com/ie3-institute/MobilitySimulator/issues/112)
- Added Bao and Staudt as Reviewers [#299](https://github.com/ie3-institute/MobilitySimulator/issues/299)
- Added ConfigFailFastSpec to properly test whether ConfigFailFast works as expected [#300](https://github.com/ie3-institute/MobilitySimulator/issues/300)
- Implemented GitHub Actions Pipeline [#351](https://github.com/ie3-institute/MobilitySimulator/issues/351)
- Added some infrastructure for GHA pipeline [#371](https://github.com/ie3-institute/MobilitySimulator/issues/371)

### Changed
- Changed sets of evs from sorted to unsorted [#113](https://github.com/ie3-institute/MobilitySimulator/issues/113)
- Changed sets of evs to sequences for speed improvement [#135](https://github.com/ie3-institute/MobilitySimulator/issues/135)
- Cleaning up `build.gradle`, updating gradle to 8.2.1 [#164](https://github.com/ie3-institute/MobilitySimulator/issues/164)
- Removed the possibility of stochastic charging so evs are always sent to the co-simulation, which should be in charge of charging the cars [#118](https://github.com/ie3-institute/MobilitySimulator/issues/118)
- Changed to updated PowerSystemDataModel V5.0.1 [#249](https://github.com/ie3-institute/MobilitySimulator/issues/249)
- Change from quantities to squants [#257](https://github.com/ie3-institute/MobilitySimulator/issues/257)
- MobSim informs EVCS in SIMONA about estimated next arrivals [#254](https://github.com/ie3-institute/MobilitySimulator/issues/254)
- Updated `Gradle` to version V8.10 [#282](https://github.com/ie3-institute/MobilitySimulator/issues/282)
- Changed Spotless to format with trailing commas [#301](https://github.com/ie3-institute/MobilitySimulator/issues/301)
- Adapting to recent changes in the API [#303](https://github.com/ie3-institute/MobilitySimulator/issues/303)
- Write all POIs on initialisation by poiWriter [#346](https://github.com/ie3-institute/MobilitySimulator/issues/346)
- Remove entry uuid from MobilitySim outputs [#341](https://github.com/ie3-institute/MobilitySimulator/issues/341)
- Remove EvcsWriter [#343](https://github.com/ie3-institute/MobilitySimulator/issues/343)
- Upgraded to scala3 [#357](https://github.com/ie3-institute/MobilitySimulator/issues/357)
- Fixed script entry in `build.gradle` [#373](https://github.com/ie3-institute/MobilitySimulator/issues/373)
- Removed Jenkins due to redundancy with GHA [#388](https://github.com/ie3-institute/MobilitySimulator/issues/388)

### Fixed
- Adapting to SIMONA-API changes introduced by [PR#37](https://github.com/ie3-institute/simonaAPI/pull/37) [[#7](https://github.com/ie3-institute/MobilitySimulator/issues/7)]
- We now track destination poi types explicitly [#85](https://github.com/ie3-institute/MobilitySimulator/issues/85)
- Fixed unintentional filtering of nearest cs with equal distance [#125](https://github.com/ie3-institute/MobilitySimulator/issues/125)
- Fixed exception messages of probability factories [#263](https://github.com/ie3-institute/MobilitySimulator/issues/263)
- MobSim sends arrivals for unexpected tick to SIMONA [#316](https://github.com/ie3-institute/MobilitySimulator/issues/316)
- Fixed movement writer [#363](https://github.com/ie3-institute/MobilitySimulator/issues/363)
- Fixed version check for dependabot PRs [#375](https://github.com/ie3-institute/MobilitySimulator/issues/375)
[Unreleased]: https://github.com/ie3-institute/MobilitySimulator
