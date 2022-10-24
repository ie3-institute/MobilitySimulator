# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- Changed sets of evs from sorted to unsorted [#113](https://github.com/ie3-institute/MobilitySimulator/issues/113)
- Removed the possibility of stochastic charging so evs are always sent to the co-simulation, which should be in charge of charging the cars [#118](https://github.com/ie3-institute/MobilitySimulator/issues/118)

### Fixed
- Adapting to SIMONA-API changes introduced by [PR#37](https://github.com/ie3-institute/simonaAPI/pull/37) [[#7](https://github.com/ie3-institute/MobilitySimulator/issues/7)]

[Unreleased]: https://github.com/ie3-institute/MobilitySimulator
