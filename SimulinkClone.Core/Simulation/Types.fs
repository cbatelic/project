namespace SimulinkClone.Core.Simulation

/// Vrijeme u sekundama
type Time = float

/// Diskretni korak
type Dt = float

/// Jedan uzorak signala u vremenu
type Sample<'T> =
    { t: Time
      value: 'T }

/// Stateful blok: ima state i step funkciju koja vraća output + novi state
type StatefulBlock<'In,'Out,'State> =
    { init: 'State
      step: Dt -> 'State -> 'In -> 'Out * 'State }

/// Stateless blok: samo funkcija
type StatelessBlock<'In,'Out> = 'In -> 'Out