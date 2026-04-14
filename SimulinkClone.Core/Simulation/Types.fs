namespace SimulinkClone.Core.Simulation

type Time = float

type Dt = float

type Sample<'T> =
    { t: Time
      value: 'T }

type StatefulBlock<'In,'Out,'State> =
    { init: 'State
      step: Dt -> 'State -> 'In -> 'Out * 'State }

type StatelessBlock<'In,'Out> = 'In -> 'Out