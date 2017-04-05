urnActStages :: UrnActType -> Set UrnStage
urnActStages = \case
  Give  -> [Pride, Anger, Worry,  Panic,  Grief,  Dread  ]
  Shut  -> [Pride, Anger, Worry,  Panic                  ]
  Lock  -> [Pride, Anger, Worry,  Panic                  ]
  Wipe  -> [Pride, Anger, Worry,  Panic                  ]
  Free  -> [Pride, Anger                                 ]
  Draw  -> [Pride                                        ]
  Bite  -> [                      Panic                  ]
  Grab  -> [                              Grief          ]
  Plop  -> [                                      Dread  ]

urnStageActs :: UrnStage -> Set UrnActType
urnStageActs = \case
  Pride -> [Give, Shut, Lock, Wipe, Free, Draw                   ]
  Anger -> [Give, Shut, Lock, Wipe, Free                         ]
  Worry -> [Give, Shut, Lock, Wipe                               ]
  Panic -> [Give, Shut, Lock, Wipe,             Bite             ]
  Grief -> [Give,                                     Grab       ]
  Dread -> [Give,                                           Plop ]

data RiskChange = SameRisk | MoreRisk | LessRisk | ZeroRisk

urnActRiskChange :: UrnAct -> Maybe Ordering
urnActRiskChange = \case
  Give -> SameRisk
  Shut -> LessRisk
  Lock -> LessRisk
  Wipe -> LessRisk
  Free -> MoreRisk
  Draw -> MoreRisk
  Bite -> ZeroRisk
  Grab -> ZeroRisk
  Plop -> ZeroRisk
