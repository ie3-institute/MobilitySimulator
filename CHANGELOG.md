# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Changed sets of evs from sorted to unsorted [#113](https://github.com/ie3-institute/MobilitySimulator/issues/113)
- Changed sets of evs to sequences for speed improvement [#135](https://github.com/ie3-institute/MobilitySimulator/issues/135)
- Cleaning up `build.gradle`, updating gradle to 8.2.1 [#164](https://github.com/ie3-institute/MobilitySimulator/issues/164)
  
### Fixed
- Adapting to SIMONA-API changes introduced by [PR#37](https://github.com/ie3-institute/simonaAPI/pull/37) [[#7](https://github.com/ie3-institute/MobilitySimulator/issues/7)]
- We now track destination poi types explicitly [#85](https://github.com/ie3-institute/MobilitySimulator/issues/85)
- Fixed unintentional filtering of nearest cs with equal distance [#125](https://github.com/ie3-institute/MobilitySimulator/issues/125)

[Unreleased]: https://github.com/ie3-institute/MobilitySimulator
