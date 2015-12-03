=PAGE=
7.1  NASTRAN DICTIONARY

   This section contains descriptions of mnemonics, acronyms, phrases, and
other commonly used NASTRAN terms. The first column of the dictionary contains
the NASTRAN terms in alphabetical order. The second column contains a code
indicating a general category for each term. The codes and categories, along
with general references to the Programmer's Manual (PM) and User's Manual
(UM), are as follows: 

   CODE    CATEGORY                                   GENERAL REFERENCE

   DBM     Data Block - Matrix                                     PM-2
   DBML    Data Block - Matrix List                                PM-2
   DBS     Data Block - Substructure Item                       UM-1.10
   DBT     Data Block - Table                                      PM-2
   EM      Executive Module                                      UM-5.7
   FMA     Functional Module - Aero                                PM-4
   FMH     Functional Module - Heat                                PM-4
   FMM     Functional Module - Matrix Operation                  UM-5.4
   FMS     Functional Module - Structural                          PM-4
   FMSS    Functional Module - Substructuring                    UM-5.9
   FMU     Functional Module - Utility                           UM-5.5
   FMX     Functional Module - User                              UM-5.6
   IA      Input - Executive Control                             UM-2.2
   IB      Input - Bulk Data                                     UM-2.4
   IC      Input - Case Control                                  UM-2.3
   IS      Input - Substructure Control                          UM-2.7
   L       Rigid Format Label                                UM-Vol. II
   M       Miscellaneous
   NP      NASTRAN card parameter                                UM-2.l
   P       Parameter Name                                    UM-Vol. II
   PH      Common Phrase or Term
   PU      Parameter Set by User                                 UM-2.4

The third column of the dictionary contains a definition or description of the
terms given in the first column. References to the User's Manual are indicated
by UM-i and the Programmer's Manual by PM-i, where i is the section number of
the manual. References to particular rigid formats are indicated by D-i, H-i,
or A-i, where i is the rigid format number in the DISPLACEMENT, HEAT, and AERO
approaches, respectively.
=PAGE=
A             P      Parameter value used to control utility module MATGPR print
                     of A-set matrices.

ABFL          DBM    [A    ] - Hydroelastic boundary area factor matrix.
                       b,fl

ABFLT         DBM    Transpose of [A    ].
                                    b,fl

ACCE          IC     Abbreviated form of ACCELERATION.

ACCE          IS     Acceleration output requests.

ACCELERATION  IC     Output request for acceleration vector. (UM-2.3, 4.2)

ACPT          DBT    Aerodynamic Connection and Property Data.

Active column PH     Column containing at least one nonzero term outside the
                     band.

ADD           FMM    Functional module to add two matrices together.

ADD           M      Parameter constant used in utility module PARAM.

ADD5          FMM    Functional module to add up to five matrices together.

ADR           FMS    Aerodynamic data recovery.

ADUMi         IB     Defines attributes of dummy elements 1 through 9.

AEFACT        IB     Used to input lists of real numbers for aeroelastic
                     analysis.

AERO          DBT    Aerodynamic Matrix Generation Data.

AERO          IB     Gives basic aerodynamic parameters.

AEROF         IC     Aerodynamic force output request.

AEROFORCE     IC     Requests frequency dependent aerodynamic loads on
                     interconnection points in aeroelastic response analysis.

AJJL          DBML   Aerodynamic Influence Matrix List.

ALL           IC     Output request for all of a specified type of output.

ALLEDGE TICS  IC     Request tic marks on all edges of X-Y plot.

ALTER         IA     Alter statement for DMAP or rigid format.

ALWAYS        P      Parameter set to -1 by a PARAM statement.

AMG           FMA    Aerodynamic Matrix Generator.

AMP           FMA    Aerodynamic Matrix Processor.

AND           M      Parameter constant used in executive module PARAM.

AOUT$         M      Indicates restart with solution set output request.

APD           FMA    Aerodynamic pool distributor and element generator.

APP           IA     Control card which specifies approach (DISP, DMAP, HEAT, or
                     AERO).

APP           P      Approach flag used for modules with several functions.

APPEND        M      File may be extended (see FILE).

ASDMAP        FMSS   Assemble substructure DMAP.

ASET          IB     Analysis set coordinate definition card.

ASET1         IB     Analysis set coordinate definition card.

AUTO          IC     Requests X-Y plot of autocorrelation function.

AUTO          DBT    Autocorrelation function table.

AXES          IC     Defines orientation of object for structure plot.

AXIC          DBT    Generated by Input File Processor 3 (IFP3) for axisymmetric
                     conical shell problems.

AXIC          IB     Axisymmetrical conical shell definition card. When this
                     card is present, most other bulk data cards may not be
                     used.

AXIF          IB     Controls the formulation of a hydroelastic problem.

AXISYM$       M      Indicates restart with conical shell or hydroelastic
                     elements.

AXISYMMETRIC  IC     Selects boundary conditions for axisymmetric shell problems
                     or specifies the existence of hydroelastic fluid harmonics.

AXSLOT        IB     Controls the formulation of acoustic analysis problems.
=PAGE=
B             PH     Upper semi band of matrix.

                       2
B2DD          DBM    [B  ] - Partition of direct input damping matrix.
                       dd

                       2
B2PP          DBM    [B  ] - Direct input damping matrix for all physical
                       pp
                     points.

B2PP          IC     Selects direct input structural damping or thermal
                     capacitance matrices.

B2PP$         M      Indicates restart with change in direct input damping
                     matrices.

BAA           DBM    [B  ] - Partition of damping matrix.
                       aa

BALL EDGE     IC     Request for all edge tic marks to be plotted on lower frame
TICS                 of an X-Y plot.

BAR           IC     Requests structure plot for all bar elements.

BAROR         IB     Bar orientation default definition.

BASIC         IS     Basic substructure for output requests.

BBAR          PH     Lower semi band of matrix.

BDD           DBM    [B  ] - Damping matrix used in direct formulation of
                       dd    dynamics problems (D-7 through D-9, A-11).

BDEBA         P      Parameter used to indicate equivalence of BDD and BAA.

BDPOOL        DBT    Hydroelastic boundary description table.

BDYC          IB     Combination of substructure boundary sets of retained
                     degrees of freedom or fixed degrees of freedom for modes
                     calculation.

BDYLIST       IB     Structure-fluid hydroelastic boundary definition.

BDYS          IB     Boundary set definition for substructuring.

BDYS1         IB     Alternate boundary set definition for substructuring.

BEGIN         EM     The first DMAP statement is always BEGIN.

BEGIN BULK    IB     Control card which marks the end of the case control deck.
                     Cards following this card are assumed to be bulk data
                     cards.

BETAD         PU     Factor in integration algorithm in transient heat transfer
                     analysis.

BFF           DBM    [B  ] - Partition of damping matrix.
                       ff

BGG           DBM    [B  ] - Damping matrix generated by Structural Matrix
                       gg    Assembler.

BGPA          DBT    Basic Grid Point Definition Table - aerodynamics.

BGPDT         DBT    Basic Grid Point Definition Table.

BGSS          DBS    Basic grid point coordinates.

BHH           DBM    [B  ] - Partition of damping matrix.
                       hh

BKL0          P      Constant parameter value used in functional module SDR2 in
                     the Buckling Analysis (D-5) and Normal Modes with
                     Differential Stiffness (D-13) Rigid Formats.

BKL1          P      Constant parameter value used in functional module SDR2 in
                     the Buckling Analysis Rigid Format (D-5).

BLANK FRAMES  IC     Requests blank frames between structure plots (UM-4.1).

BLEFT TICS    IC     Request for left edge tic marks to be plotted on bottom
                     frame of an X-Y plot.

BMG           FMS    Generates DMIG card images describing interconnection of
                     fluid and structure.

BMTX          DBS    Viscous damping matrix.

BNN           DBM    [B  ] - Partition of damping matrix.
                       nn

BOTH          IC     Bulk data echo option - Requests both unsorted and sorted
                     printout of bulk data deck.

BOUNDARY      IS     Defines set of retained degrees of freedom.

BOV           P      Aerodynamic parameter equal to the reference semichord
                     divided by velocity.

BPI           IC     Bits per inch - Plot tape density must be specified on
                     control cards in addition to this data card. The required
                     value will vary from one installation to another.

BQG           DBM    Single-point forces of constraint for a Buckling Analysis
                     problem (D-5).

BRECOVER      IS     Basic Substructure Data Recovery.

BRIGHT TICS   IC     Request for right edge tick marks to be plotted on bottom
                     frame for X-Y plot.

BSHH          DBM    Total modal damping matrix - h set.

BUCKLING      IA     Selects rigid format for buckling analysis.

BUCKLING      P      Constant parameter value used in functional module READ in
                     the Buckling Analysis Rigid Format (D-5).

BUCKLING      P      Used in printing rigid format error messages for Buckling
                     Analysis (D-5).

BUFFSIZE      NP     Defines the number of words in a GINO buffer.

Bulk Data     PH     One of the data decks necessary to run a problem under the
Deck                 NASTRAN system. This deck begins after the BEGIN BULK card
                     and ends with the ENDDATA card, and contains the data of
                     the mathematical model. The format of each bulk data card
                     is fixed field, 8 or 16 columns for each value.
=PAGE=
C             M      Used in parameter section of DMAP statement. Indicates that
                     parameter is a constant.
                                                                           _
C             PM     Symbol for active column in triangular decomposition (C
                     used for active rows).

CAERO1        IB     Aerodynamic panel element, doublet lattice theory.

CAERO2        IB     Aerodynamic body element, doublet lattice theory.

CAERO3        IB     Aerodynamic surface element, Mach box.

CAERO4        IB     Aerodynamic macro element, strip theory.

CAERO5        IB     Aerodynamic macro element, piston theory.

CALCOMP       IC     Request California Computer plotter.

CAMERA        IC     Selects one or both of the two cameras for the SC 4020
                     cathode ray tube electronic plotter. This information must
                     usually also be given to the plotter operator on the run
                     submittal slip, which will vary from one installation to
                     another. (UM-4)

CARDNO        P      Parameter used to accumulate a count of all card output
                     punched except the NASTRAN restart dictionary.

CASE          FMS    Extracts user request from CASECC for current loop in
                     dynamics rigid formats (D-7 through D-12).

Case Control  PM     One of the data decks necessary to run a problem under the
Deck                 NASTRAN system. It contains cards which select particular
                     data sets from the Bulk Data Deck, output request cards,
                     and titling information. Cards in this deck are free field.

CASECC        DBT    Case control data block.

CASEXX        DBT    Case control data block as modified by functional module
                     CASE.

CASEYY        DBT    Appended case control data table.

CASEZZ        DBT    CASEYY reduced to OFREQ list.

CAXIF2        IB     Acoustic core element connection definition card.

CAXIF3        IB     Acoustic triangular element connection definition card.

CAXIF4        IB     Acoustic quadrilateral element connection definition card.

CBAR          IB     Bar element connection definition card.

CCONEAX       IB     Axisymmetrical conical shell element connection card.

CDAMP1        IB     Scalar damper connection definition card.

CDAMP2        IB     Scalar damper property and connection definition card.

CDAMP3        IB     Scalar damper connection definition card (connecting scalar
                     points).

CDAMP4        IB     Scalar damper property and connection definition card
                     (connecting scalar points).

CDUMi         IB     Defines definition card for dummy elements 1 through 9.

CEAD          FMS    Complex Eigenvalue Analysis - Displacement.

CEIF          P      Parameter used in SDR2 in Complex Eigenvalue Analysis (0-7
                     and 0-10).

CEIGN         P      Parameter used in VDR in Complex Eigenvalue Analysis (0-7
                     and 0-10).

CELAS1        IB     Scalar spring connection definition card.

CELAS2        IB     Scalar spring property and connection definition card.

CELAS3        IB     Scalar spring connection definition card (connecting scalar
                     points).

CELAS4        IB     Scalar spring property and connecting definition card
                     (connecting scalar points).

CEND          IA     The last card of the Executive Control Deck.

CFLUID2       IB     Fluid core element connection definition card.

CFLUID3       IB     Fluid triangular element connection definition card.

CFLUID4       IB     Fluid quadrilateral element connection definition card.

CHBDY         IB     Boundary element connection definition card for heat
                     transfer analysis.

CHECK         IB     Checks contents of external file.

Checkpoint    PH     The process of writing selected data blocks onto the New
                     Problem Tape for subsequent restarts.

CHEXA1        IB     Hexahedron element connection definition card - five
                     tetrahedra.

CHEXA2        IB     Hexahedron element connection definition card - ten
                     tetrahedra.

CHKPNT        EM     Checkpoint module.

CHKPNT        IA     Request for checkpoint execution.

CLAMA         DBT    Complex eigenvalue output table.

CLAMAL        DBT    Appended case control data table.

CLANAL1       DBT    CLAMAL reduced to OFREQ list.

CLEAR         IC     Causes all parameter values used for X-Y plots to be reset
                     to their default values except plotter and the titles (UM-
                     4.2).

CMASS1        IB     Scalar mass connection definition card.

CMASS2        IB     Scalar mass property and connection definition card.

CMASS3        IB     Scalar mass connection definition card (connecting scalar
                     points).

CMASS4        IB     Scalar mass property and connection definition card
                     (connecting scalar points).

CMETHOD       IC     Complex eigenvalue analysis method selection.

CMETHOD$      M      Indicates restart with change in complex eigenvalue
                     analysis method selection.

CMPLEV        P      Parameter used in GKAD to indicate complex eigenvalue
                     problem.

Cold Start    PH     A NASTRAN problem initiated at its logical beginning. A
                     cold start will never use an Old Problem Tape but it may
                     create a New Problem Tape for subsequent restarts.

COLOR         IC     Selects ink color for table plotters (UM-4.l).

COMBINE       IB     Combines sets of substructures.

COMB1         FMSS   Substructure Combination, Step 1.

COMB2         FMSS   Substructure Combination, Step 2.

COMPONENT     IB     Identifies component substructure for special processing.

CONCT         IB     Specifies grid points and degrees of freedom for manually
                     specified connectivities using substructuring - will be
                     overridden by RELAS data.

CONCT1        IB     Alternate specification of connectivities using
                     substructuring.

COND          EM     Conditional transfer.

CONFIG        NP     Defines the model number of the computer system
                     configuration for use in timing equations.

CONM1         IB     Structural mass element connection definition card.

CONM2         IB     Structural mass element connection definition card.

CONNECT       IB     Defines sets for manually connected grids and releases.

CONROD        IB     Rod element property and connection definition card.

CONROD        IC     Requests structure plot for all CONROD elements.

CONT          L      Continue if [K  ] is nonsingular.
                                   oo

CONTINUE      L      Exit after last loop.

CONTOUR       IC     Specifies displacement and stress contours to be drawn on
                     structure plots.

COPY          FMU    Generates a physical copy of a data block.

CORD1C        IB     Cylindrical coordinate system definition (by grid point
                     ID).

CORD1R        IB     Rectangular coordinate system definition (by grid point
                     ID).

CORD1S        IB     Spherical coordinate system definition (by grid point ID).

CORD2C        IB     Cylindrical coordinate system definition (by coordinates).

CORD2R        IB     Rectangular coordinate system definition (by coordinates).

CORD2S        IB     Spherical coordinate system definition (by coordinates).

COSINE        IC     Indicates cosine boundary conditions for conical shell
                     problem.

COUPMASS      PU     Parameter used to request coupled mass.

CPBAR         PU     Selects coupled mass option for BAR element.

CPHID         DBM    Complex eigenvectors - solution set.

CPHIA         DBM    Complex eigenvector matrix, A-set.

CPHIH1        DBM    PHIHL reduced to OFREQ list.

CPHIK         DBM    Complex eigenvector matrix, aerodynamic box points.

CPHIP         DBM    Complex eigenvectors - physical set.

CPHIPA        DBM    Complex eigenvector matrix, PA-set.

CPHIPS        DBM    Complex eigenvector matrix, PS-set.

CPQDPLT       PU     Selects coupled mass option for QDPLT element.

CPQUAD1       PU     Selects coupled mass option for QUAD1 element.

CPQUAD2       PU     Selects coupled mass option for QUAD2 element.

CPROD         PU     Selects coupled mass option for ROD and CONROD elements.

CPTRBSC       PU     Selects coupled mass option for TRBSC element.

CPTRIA1       PU     Selects coupled mass option for TRIA1 element.

CPTRIA2       PU     Selects coupled mass option for TRIA2 element.

CPTRPLT       PU     Selects coupled mass option for TRPLT element.

CPTUBE        PU     Selects coupled mass option for TUBE element.

CQDMEM        IB     Quadrilateral membrane element connection definition card.

CQDMEM1       IB     Isoparametric quadrilateral membrane element connection
                     definition card.

CQDMEM2       IB     Quadrilateral membrane element connection definition card.

CQDPLT        IB     Quadrilateral bending element connection definition card.

CQUAD1        IB     General Quadrilateral element connection definition card.

CQUAD2        IB     Homogeneous quadrilateral element connection definition
                     card.

CRIGD1        IB     Rigid Element Connection.

CRIGD2        IB     Rigid Element Connection.

CRIGD3        IB     General rigid element connection.

CRIGDR        IB     Rigid Rod element connection.

CROD          IB     Rod element connection definition card.

CREDUCE       IS     Complex modal reduction request.

CSHEAR        IB     Shear panel element connection definition card.

CSLOT3        IB     Triangular slot element connection definition card for
                     acoustic analysis.

CSLOT4        IB     Quadrilateral slot element connection definition card for
                     acoustic analysis.

CSTM          DBS    Local coordinate system transformation matrices.

CSTM          DBT    Coordinate System Transformation Matrices.

CSTMA         DBT    Coordinate System Transformation Matrices - Aerodynamics.

CTETRA        IB     Tetrahedron element connection definition card.

CTORDRG       IB     Toroidal ring element connection card.

CTRAPRG       IB     Trapezoidal ring element connection card.

CTRBSC        IB     Basic bending triangular element connection definition
                     card.

CTRIA1        IB     General triangular element connection definition card.

CTRIA2        IB     Homogeneous triangular element connection definition card.

CTRIARG       IB     Triangular ring element connection card.

CTRIM         IB     Linear strain triangular element connection.

CTRMRM        IB     Triangular membrane element connection definition card.

CTRPLT        IB     Triangular bending element connection definition card.

CTRPLT1       IB     Triangular element connection.

CTRSHL        IB     Triangular shell element connection.

CTUBE         IB     Tube element connection definition card.

CTWIST        IB     Twist panel element connection definition card.

CTYPE         PU     Defines the type of cyclic symmetry.

CURVLINE      IC     Request to connect points with lines and/or to use symbols
SYMBOL               for X-Y plots.

CVISC         IB     Viscous damper element connection definition card.

CWEDGE        IB     Wedge element connection definition card.

CYCIO         PU     A parameter which specifies the form of the input and
                     output data using cyclic symmetry.

CYCSEQ        PU     A parameter which specifies the procedure for sequencing
                     the equations in the solution set using cyclic symmetry.
=PAGE=
D             P      Parameter value used to control utility module MATGPR
                     print of solution set matrices.

DAREA         IB     Dynamic load scale card.

DAREAS        IB     Dynamic load scale card for substructuring.

Data Block    PH     Designates a set of data (matrix, table) occupying a file.
                     A file is "allocated" to a data block and a data block is
                     "assigned" to a file.

Data Pool     PN     An executive file containing the OSCAR and any data blocks
File                 pooled by the Executive Segment File Allocator (XSFA)
                     module. The contents of this file are described within the
                     data pool dictionary (DPL).

DDR           FMX    User dummy module.

DDR1          FMS    Dynamic Data Recovery - Phase 1.

DDR2          FMS    Dynamic Data Recovery - Phase 2.

DDRMM         FMS    Dynamic data recovery, matrix method.

Deck          PH     1. Job Control
                     2. Executive Control Deck
                     3. Substructure Control Deck
                     4. Case Control Deck
                     5. Bulk Data Deck

DECOMOPT      P      Controls type of arithmetic used in the decomposition for
                     frequency-response problems.

DECOMP        FMM    To decompose a square matrix into upper and lower
                     triangular factors.

Default       PH     Many NASTRAN data items have default values supplied by the
                     system. For example, the default value for MAXLINES is
                     20000.

DEFORM        IB     Enforced element deformation definition card.

DEFORM        IC     Enforced element deformation set selection.

DEFORM$       M      Indicates restart with change in enforced element
                     deformation selection.

DEFORMATION   IC     Indicates subcases to be used for deformed structure plots.

DELAY         IB     Dynamic load time delay card.

Delete        IB     Delete cards from Bulk Data Deck.

DELETE        IS     Deletes individual substructure items from the SOF.

DELTAPG       DBM    Incremental load vector in Piecewise Linear Analysis Rigid
                     Format (D-6).

DELTAQG       DBM    Incremental vector of single point constraint forces in the
                     Piecewise Linear Analysis Rigid Format (D-6).

DELTAUGV      DBM    Incremental displacement vector in the Piecewise Linear
                     Analysis Rigid Format (D-6).

DENSITY       IC     Density of lines for SC 4020 plotter.

DENSITY       IC     Plot tape density must be specified to plotter operator on
                     run submittal form and will vary from one installation to
                     another (UM-4.1).

DESTROY       IS     Removes all data referencing a component substructure.

DESTRY        P      Appended AJJL parameter.

DET           IB     Eigenvalue analysis method option - determinant (see EIGR,
                     EIGB, EIGC).

DET           P      Scaled determinant |K  |, see NDET.
                                          oo

DIAGONAL      FMU    Strips diagonal from matrix.

DIFF          P      Parameter used in the Piecewise Linear Analysis Rigid
                     Format (D-6).

DIFFERENTIAL  IA     Selects rigid format for static analysis with differential
STIFFNESS            stiffness.

DIFFSTIF      P      Parameter used in the PRTPARM module in the Differential
                     Stiffness Rigid Format (D-4).

DIRCEAD       P      Used in printing rigid format error messages for direct
                     complex eigenvalue analysis (D-7).

DIRECT        P      Parameter used to indicate direct formulation of dynamics
                     problems (D-7 through D-9).

DIRECT        IA     Selects rigid format for direct complex eigenvalue
COMPLEX              analysis.
EIGENVALUES

DIRECT        IA     Selects rigid format for direct frequency and random
FREQUENCY            response.
RESPONSE


DIRECT        IA     Selects rigid format for direct transient response.
TRANSIENT
RESPONSE

DIRFRRD       P      Used in printing rigid format error messages for direct
                     frequency response.

DIRTRD        P      Used in printing rigid format error messages for direct
                     transient response (D-9).

DISP          IA     Displacement approach to structural analysis.

DISP          IC     Abbreviated form of DISPLACEMENT.

DISP          IS     Displacement output request.

DISPLACEMENT  IC     Request for output of displacement vector or eigenvector.
                     (UM-2.3, 4.2).

DIT           DBT    Direct Input Table.

DIV           P      Parameter constant used in utility module PARAM.

DLOAD         IB     Dynamics load assembly definition.

DLOAD         IC     Dynamic load set solution request.

DLOAD$        M      Indicates restart with change in dynamic load set request.

DLT           DBT    Dynamic Loads Table.

DM            DBM    [D] - Rigid body transformation matrix.

DMAP          IA     Approach option (Direct Matrix Abstraction Program).

DMAP          PH     A statement in the DMAP Language.
Instruction

DMAP Language PH     Data block-oriented language used by the NASTRAN Executive
                     System to direct the sequence and flow of modules to be
                     executed.

DMAP Loop     PH     A DMAP sequence to be repeated, initiated with a LABEL DMAP
                     instruction and terminated by a REPT DMAP instruction.

DMAP Module   PH     A module called by means of a DMAP instruction.

DMAP Sequence PH     A set of DMAP instructions.

DMI           IB     Direct Matrix Input (data block is defined and used by
                     you).

DMIAX         IB     Direct Matrix Input - Axisymmetric, used in dynamic rigid
                     formats (D-7 through D-12).

DMIG          IB     Direct Matrix Input - used in dynamic rigid formats (D-7
                     through D-12).

Doublet       PH     Subsonic aerodynamic theory.
Lattice

DPD           FMS    Dynamic Pool Distributor.

DPH           M      Data Pool Housekeeper - Executive routine.

DPHASE        IB     Dynamic load phase lead card.

DSO           P      Parameter used in functional module SDR2 in the
                     Differential Stiffness Rigid Format (D-4).

DS1           P      Parameter used in functional module SDR2 in the
                     Differential Stiffness Rigid Format (D-4).

DSCO          IC     Abbreviated form of DSCOEFFICIENT.

DSCO$         M      Indicates restart with change in differential stiffness
                     load factor.

DSCOEFFICIENT IC     Selects loading factor for normal modes with differential
                     stiffness.

DSCOSET       P      Differential Stiffness coefficient set number. Used in the
                     Differential Stiffness Rigid Format (D-13).

DSFACT        IB     Differential stiffness factor set definition card.

DSMG1         EMS    Differential Stiffness Matrix Generator - Phase 1.

DSMG2         FMS    Differential Stiffness Matrix Generator - Phase 2.

DTI           IB     Direct Table Input - means by which you may directly input
                     any table data block.

DUMMOD1       FMX    Dummy Module-1.

DUMMOD2       FMX    Dummy Module-2.

DUMMOD3       FMX    Dummy Module-3.

DUMMOD4       FMX    Dummy Module-4.

Dummy Element PH     Provision for you to insert additional finite element into
                     the NASTRAN element library.

DUMP          IS     Copies the entire SOF to an external file.

Dump          PH     Printed output of contents of all, or a portion, of main
                     memory at some point in the problem solution.

DYNAMICS      DBT    Generated by the Input File Processor (IFP) for Real
                     Eigenvalue, Buckling, or any of the Dynamics Rigid Formats
                     (D-3, D-5, and D-7 through D-12).

D1JE          DBM    Downwash factors due to extra points - real.

D2JE          DBM    Downwash factors due to extra points - complex.

D1JK          DBM    Real part of downwash matrix.

D2JK          DBM    Imaginary part of downwash matrix.
=PAGE=
E             P      Parameter value used by MATGPR to print matrices associated
                     with extra points.

ECHO          IC     Output request statement for echo of bulk data.

ECPT          DBT    Element Connection and Properties Table.

ECPTNL        DBT    Nonlinear subset of the ECPT. This data block is used only
                     in the Piecewise Linear Analysis Rigid Format (D-6).

ECPTNL1       DBT    Updated version of the ECPTNL data block. Used only in the
                     Piecewise Linear Analysis Rigid Format (D-6).

ECPTNLPG      P      Error flag for the Piecewise Linear Analysis Rigid Format
                     (D-6). If all elements in a piecewise linear analysis
                     problem are linear, this error flag is set and a DMAP exit
                     occurs.

ECT           DBT    Element Connection Table.

ECTA          DBT    Element Connection Table - Aerodynamics.

EDIT          IS     Removes data from SOF file.

EDT           DBT    Enforced Deformation Table - generated by Input File
                     Processor.

EED           DBT    Eigenvalue Extraction Data table (D-3, D-5, D-7, D-10, D-
                     11, D-12, D-13, D-15, A-10, A-11).

EIGB          IB     Real eigenvalue extraction data for buckling analysis (D-5).

EIGC          IB     Complex eigenvalue extraction data card (D-7 and D-10).

EIGP          IB     Complex eigenvalue pole definition card (D-7 and D-10).

EIGR          IB     Real eigenvalue extraction data for normal mode analysis
                     (D-3, D-10 through D-13, D-15, A-10).

EIGVS         P      Number of eigenvalues found by CEAD module.

ELEMENTS      IC     Used in element set definition for structure plot.

ELFORCE       IC     Requests the forces in a set of structural elements or the
                     temperature gradients and fluxes in a set of structural or
                     heat elements in heat transfer.

ELSETS        DBT    Element plot set connection tables.

ELSETSA       DBT    Data block ELSETS, extended to include generated
                     aerodynamic elements.

ELSTRESS      IC     Request for output of element stresses. (UM-2.3, 4.2)

END           EM     The last DMAP statement is always END.

END           IA     END is the last statement in all DMAP sequences.

ENDALTER      IA     Last card of alter packet.

ENDDATA       IB     End of Bulk Data Deck.

ENDSUBS       IS     Terminates the Substructure Control Deck.

ENERGY        IS     Modal energies output requests.

EOF           PH     End-of-File.

EPOINT        IB     Extra point definition card - used in dynamics problems
                     only.

EPSHT         PU     Used in convergence tests for nonlinear heat transfer
                     analysis.

EPSILON       PH     Error ratio computed in SSG3. î  = î  if the referenced
SUB E (î )                                          e    l
        e            load is {P } and î  = î  if the referenced load is {P }.
                               l       e    o                             o
                     See Volume Il, Section 2.1.2, for mathematical definition
                     of î  and î .
                         o      l

EPSIO         PU     A parameter to test the convergence of iterated
                     differential stiffness.

EPT           DBT    Element Property Table - output by Input File Processor.

EQAERO        DBT    Equivalence between external points and scalar index values
                     - Aerodynamics.

EQDYN         DBT    Equivalence of internal and external indices - dynamics.

EQEXIN        DBT    Equivalence of internal and external indices.

EQSS          DBS    External grid point and internal point equivalence data.

EQUIV         EM     Equivalence data blocks.

EQUIV         IS     Creates a new equivalent substructure.

Equivalence   PH     Data blocks are considered equivalenced when references to
                     their equivalent names access the same physical data file.

ERROR1        L      Label used when rigid format errors are detected.

ERROR2        L      Label used when rigid format errors are detected.

ERROR3        L      Label used when rigid format errors are detected.

ERROR4        L      Label used when rigid format errors are detected.

ERROR5        L      Label used when rigid format errors are detected.

ERROR6        L      Label used when rigid format errors are detected.

ESE           IC     Request for element strain energy output.

EST           DBT    Element Summary Table.

ESTL          DBT    Element Summary Table for Linear elements. Used only in the
                     Piecewise Linear Analysis Rigid Format (D-6).

ESTNL         DBT    Element Summary Table for Nonlinear elements. Used only in
                     the Piecewise Linear Analysis Rigid Format (D-6).

ESTNL1        DBT    Updated version of the ESTNL data block. Used only in the
                     Piecewise Linear Analysis Rigid Format (D-6).

EVEC          DBM    Partitioning vector. D-set to A and E.

EXCEPT        IC     Forms exceptions to string of values in set declarations.

EXCLUDE       IC     Used in set definition for structure plots.

Executive     PH     1. Executive Control Deck
                     2. NASTRAN Executive System

Executive     PH     One of the data decks necessary to run a problem under the
Control              NASTRAN system. This deck begins with the ID card and ends
Deck                 with the CEND card. Among other things, cards in this deck
                     select the solution approach and rigid format to be used,
                     limit the execution time, and control checkpointing and
                     restart.

Executive     PH     The Executive System initiates a NASTRAN problem solution
System               via the Preface, allocates files to data blocks during
                     problem solution, controls the sequence of the modules to
                     be executed, and provides for problem restart capability.

EXIO          FMSS   External input/output for the SOF.

EXIT          EM     Program termination DMAP statement.

External Sort PH     Order of grid, scalar and extra points determined by your
                     numerical order of point identification.

Extra Point   PH     A "point" which is defined on an EPOINT bulk data card. An
                     extra point has no geometrical coordinates, defines only
                     one degree of freedom of the model, and is used only in
                     dynamics solutions.
=PAGE=
F             P      Parameter value used by MATGPR to print F-set matrices.

FA1           FMS    Flutter Analysis - Phase 1.

FA2           FMS    Flutter Analysis - Phase 2.

FBS           FMM    Forward and Backward Substitution.

FE            P      Parameter used by MATGPR to print out FE-set matrices.

FEER          IB     Fast Eigenvalue Extraction Routine eigensolution method.

FIAT          M      File Allocation Table. Core resident executive table where
                     data block names, status of the data blocks (assigned to a
                     file, purged, equivalenced, etc.) and trailer for the data
                     blocks are stored.

FILE          EM     Defines special data block characteristics to DMAP
                     compiler.

FILE          IA     Term appearing on the checkpoint dictionary cards
                     indicating the file number (internal) associated with a
                     particular data block.

FILE          M      The FILE DMAP statement specifies data block
                     characteristics such as TAPE, SAVE, and APPEND.

FILE          PH     Designates an auxiliary storage area or unit.

FILES         NP     Declares the NASTRAN permanent files as disk files.

FIND          IC     Selects parameters for structure plot.

FINIS         L      Label used in all displacement rigid format DMAPs to
                     terminate execution of DMAP.

Finite        PH     Idealized unit of a structural model that represents the
Element              distributed elastic properties of a structure.

FIST          M      File Status Table. Core resident executive table where
                     internal file names and pointers to the FIAT, pertaining
                     only to the module being executed, are stored.

FIXED         IB     Defines set of constrained degrees of freedom for modes
                     calculation.

FLAGS         IA     Term appearing on the checkpoint dictionary cards
                     indicating the status of a data block (equivalenced or
                     not).

FLFACT        IB     Specifies densities, Mach numbers, and frequencies.

FLIST         DBT    Flutter Control Table.

FLOOP         P      Flutter loop counter/control.

FLSYM         IB     Structural symmetry definition card for use in hydroelastic
                     problems.

FLUID         IC     Indicates hydroelastic harmonic degrees of freedom.

FLUTTER       IB     Defines flutter data.

FMETHOD       IC     Flutter Analysis Method Selection.

FADE          P      Mode number of first mode selected by you in modal
                     dynamics formulations.

FOL           DBT    Frequency response output frequencies.

FORCE         IB     Static load definition (vector).

FORCE         IC     Request for output of element forces.

FORCE1        IB     Static load definition (magnitude and two grid points).

FORCE2        IB     Static load definition (magnitude and four grid points).

FORCEAX       IB     Static load definition for conical shell problem.

FREEPT        IB     Defines point on a free surface of a fluid for output
                     purposes.

FREQ          IB     Frequency list definition.

FREQ$         M      Indicates restart with change in frequencies to be solved.

FREQ1         IB     Frequency list definition (linear increments).

FREQ2         IB     Frequency list definition (logarithmic increments).

FREQRESP      P      Parameter used in SDR2 to indicate a frequency response
                     problem.

FREQUENCY     IC     Selects the set of frequencies to be solved in frequency
                     response problems.

FREQY         P      Selects between frequency and transient in aeroelastic
                     response.

FRL           DBT    Frequency Response List.

FRLG          FMA    Frequency response load generator.

FRQSET        P      Used in FRRD to indicate user selected frequency set.

FRRD          FMS    Frequency and Random Response - Displacement approach.

FRRD2         FMA    Frequency response, with aerodynamic matrix capability.

FSAVE         DBT    Flutter Storage Save Table.

FSLIST        IB     Defines a free surface of a fluid in a hydroelastic
                     problem.

Functional    PH     An independent group of subroutines that perform a
Module               structural analysis function.
=PAGE=
G             PU     1. Parameter used by MATGPR to print G-set matrices.
                     2. Parameter used to input uniform structural damping
                        coefficient (D-7 through D-9).

GEI           DBT    General Element Input.

GENEL         IB     General element definition.

GEOM1         DBT    Geometric data input table - generated by the Input File
                     Processor.

GEOM2         DBT    Connection input table - generated by the Input File
                     Processor.

GEOM3         DBT    Static load and temperature input table - generated by the
                     Input File Processor.

GEOM4         DBT    Displacement sets definition input table - generated by the
                     Input File Processor.

GI            FMA    Geometry Interpolator.

GIMS          DBS    G transformation matrix for interior points from a modal
                     reduction.

GINO          M      General input/output. GINO is a collection of subroutines
                     which is the input/output control system for NASTRAN.

GINO Buffer   PH     Storage reserved in open core for each GINO file opened.
                     The size of the buffer is machine dependent.

GINO File     PH     File number used internally in DMAP modules to access data
Number               blocks.

GIV           IB     Eigenvalue analysis method option - Givens (see EIGR).

GKAD          FMS    General [K] Assembler - Direct.

GKAM          FMS    General [K] Assembler - Modal.

GM            DBM    [G ] - multipoint constraint transformation matrix.
                       m

                       d
GMD           DBM    [G ] - multipoint constraint transformation matrix used in
                       m
                     dynamic analysis.

GNFIAT        M      Generate FIAT. The preface routine which generates the
                     initial FIAT.

GO            DBM    [G ] - structural matrix partitioning transformation
                       o
                     matrix.

                       d
GOD           DBM    [G ] - structural matrix partitioning transformation matrix
                       o    used in dynamic analysis.

GPARAM        IS     Specifies structural damping parameter.

GP1           FMS    Geometry Processor - part 1.

GP2           FMS    Geometry Processor - part 2.

GP3           FMS    Geometry Processor - part 3.

GP4           EMS    Geometry Processor - part 4.

GPCT          DBT    Grid Point Connection Table.

GPDT          DBT    Grid Point Definition Table.

GPFORCE       IC     Requests grid point force balance output.

GPI           M      General Problem Initialization (see XGPI).

GPL           DBT    Grid Point List.

GPLA          DBT    Grid Point List - Aerodynamics.

GPLD          DBT    Grid Point List used in dynamic analysis.

GPSETS        DBT    Grid point plot sets.

GPSETSA       DBT    Data block GPSETS, extended to include generated
                     aerodynamic grid points.

GPSP          FMS    Grid Point Singularity Processor.

GPST          DBT    Grid Point Singularity Table.

GPTT          DBT    Grid Point Temperature Table.

GPWG          FMS    Grid Point Weight Generator.

GRAV          IB     Gravity vector definition card.

GRDEQ         PU     Selects the grid point about which equilibrium will be
                     checked.

GRDPNT        PU     Used in all displacement rigid formats to specify execution
                     of the grid point weight generator (GPWG) by you. A
                     positive value references a grid point of the structural
                     model. A value of zero indicates the origin of the basic
                     coordinate system.

GRDSET        IB     Grid point default definition card.

GRID          IB     Grid point definition card.

Grid Point    PH     A point in Euclidean 3-dimensional space defined on a GRID
                     bulk data card. A grid point defines 6 degrees of freedom,
                     3 translational and 3 rotational.

GRID POINTS   IC     Used in set definition for structure plots.

GRIDB         IB     Grid point definition card for hydroelastic model.

GRIDF         IB     Grid point definition card for axisymmetric fluid cavity.

GRIDS         IB     Grid point definition card for slotted acoustic cavity.

GTKA          DBM    Aerodynamic transformation matrix - k-set to a-set.

GTRAN         IB     Redefines the output coordinate system grid point
                     displacement sets.

GUST          FMA    Calculates loads due to gust.

GUST          IB     Defines stationary vertical gust.

GUST          IC     Aerodynamic gust input request.

GUSTAERO      PU     Requests matrices used only in gust calculations to be
                     computed.
=PAGE=
HARMONICS     IC     Controls number of harmonics output in axisymmetric shell
                     problems and hydroelastic problems.

                       2
HS2DD         DBM    [B  ] - Partition of heat capacity matrix.
                       dd

                       2
HS2PP         DBM    [B  ] - Partition of heat capacity matrix.
                       pp

HBAA          DBM    [B  ] - Partition of heat capacity matrix.
                       aa

HSDD          DBM    [B  ] - Partition of heat capacity matrix.
                       dd

HSFF          DBM    [B  ] - Partition of heat capacity matrix.
                       ff

HSGG          DBM    [B  ] - Heat capacity matrix.
                       gg

HSNN          DBM    [B  ] - Partition of heat capacity matrix.
                       nn

HDLT          DBT    Dynamic loads table for heat transfer analysis.

Header record PH     Initial record of a data block. Typically a header record
                     contains only 2 BCD words, the alphanumeric name of the
                     data block.

HEAT          IA     Selects heat transfer analysis on APProach card.

HESS          IB     Upper Hessenberg eigenvalue extraction method.

HFREQ         PU     High frequency limit for modal formulation of dynamics
                     problems (D-10 through D-12, A-10, A-11).

HICORE        NP     Defines the amount of open core available to you on
                     the UNIVAC 1100 series.

                       2
HK2DD         DBM    [K  ] - Partition of heat conductivity matrix.
                       dd

                       2
HK2PP         DBM    [K  ] - Partition of heat conductivity matrix.
                       pp

HKAA          DBM    [K  ] - Partition of heat conductivity matrix.
                       aa

HKDD          DBM    [K  ] - Partition of heat conductivity matrix.
                       dd

HKFF          DBM    [K  ] - Partition of heat conductivity matrix.
                       ff

HKFS          DBM    [K  ] - Partition of heat conductivity matrix.
                       fs

HKGG          DBM    [K  ] - Heat conductivity matrix, including estimated
                       gg    linear component of radiation.

                       x
HKGGX         DBM    [K  ] - Heat conductivity matrix.
                       gg

HKNN          DBM    [K  ] - Partition of-heat conductivity matrix.
                       nn

HLFT          DBS    Left side H transformation matrix from unsymmetric CREDUCE
                     operation.

HOEFlX        DBT    Heat flux output table for CHBDY elements.

HORG          DBS    H or  G transformation matrix.

                       o
HPDO          DBM    {P } - Partition of dynamic load vector.
                       d

                       t
HPDT          DBM    {P } - Partition of dynamic load vector.
                       d

                       o
HPPO          DBM    {P } - Partition of dynamic load vector.
                       p

                       o
HPSO          DBM    {P } - Partition of dynamic load vector.
                       s

HQGE          DBM    [Q  ] - Element radiation flux matrix for heat transfer
                       ge
                     analysis.

HRAA          DBM    [R  ] - Partition of radiation matrix.
                       aa

HRDD          DBM    [R  ] - Partition of radiation matrix.
                       dd

HRFF          DBM    [R  ] - Partition of radiation matrix.
                       ff

HRGG          DBM    [R  ] - Radiation matrix for heat transfer analysis.
                       gg

HRNN          DBM    [R  ] - Partition of radiation matrix.
                       nn

HSLT          DBT    Static heat flux table.

HTOL          DBT    List of output time steps for heat transfer.
=PAGE=
IC            IC     Transient analysis initial condition set selection.

ID            IA     The first card of any data deck is the identification (ID)
                     card. The two data items on this card are BCD values.

IFP           EM     Input File Processor. The preface module which processes
                     the sorted Bulk Data Deck and outputs various data blocks
                     depending on the card types present in the Bulk Data Deck.

IFP1          EM     Input File Processor 1. The preface module which processes
                     the Case Control Deck and writes the CASECC, PCDB, and
                     XYCDB data blocks.

IFP3          EM     Input File Processor 3. The preface module which processes
                     bulk data cards for a conical shell problem.

IFP4          EM     Input File Processor 4. The preface module which processes
                     bulk data cards for a hydroelastic problem.

IFT           FMA    Inverse Fourier transformation.

IFTM          PU     A parameter which selects the method for integration of the
                     Inverse Fourier Transform.

IFTSKP        L      Used to skip IFT module.

IMAG          IC     Output request for real and imaginary parts of some
                     quantity such as displacement, load, single point force of
                     constraint element force, or stress.

IMPL          P      Parameter constant used in executive module PARAM.

INCLUDE       IC     Used in set definition for structure plots.

INERTIA       P      Used in printing rigid format error messages for Static
                     Analysis with Inertia Relief (D-2).

INERTIA       IA     Selects rigid format for static analysis with inertia
RELIEF               relief.

INPT          M      A reserved NASTRAN physical file which must be set up by
                     you when used.

INPUT         FMU    Generates most of bulk data for selected academic problems.

Input Data    PH     A data block input to a module. An input data block must
Block                have been previously output from some module and may not be
                     written on.

Input Data    PH     The card input data to the NASTRAN system are in 3 sets,
Cards                the Executive Control Deck, the Case Control Deck, and the
                     Bulk Data Deck.

INPUTT1       FMU    Reads data blocks from GINO-written user tapes.

INPUTT2       FMU    Reads data blocks from FORTRAN-written user tapes.

INPUTT3       FMX    Auxiliary input file processor.

INPUTT4       FMX    Auxiliary input file processor.

Internal Sort PH     Same order as external sort except when SEQGP or SEQEP bulk
                     data cards are used to change the sequence.

INV           IB     Inverse power eigenvalue analysis option - specified on
                     EIGR, EIGB, or EIGC cards.

IRES          PU     Causes printout of residual vectors in statics rigid
                     formats when set nonnegative via a PARAM bulk data card.
                     (D-1, D-2, D-4, D-5, D-6).

ISTART        PU     A parameter which causes the alternate starting method to
                     be used in transient analysis.

ITEMS         IS     Specifies data items to be copied in or out.




JUMP          EM     Unconditional transfer DMAP statement.

JUMPPLOT      P      Parameter used by structure plotter modules PLTSET and
                     PLOT.
=PAGE=
                       2
K2DD          DBM    [K  ] - Partition of direct input stiffness matrix.
                       dd

                       2d
K2DPP         DBM    [K  ] - Direct input stiffness matrix for all physical
                       pp    points from bulk data deck.

                       2
K2PP          DBM    [K  ] - Direct input stiffness matrix for all physical
                       pp    points.

K2PP          IC     Selects direct input structural stiffness or thermal
                     conductance matrices.

K2PP$         M      Indicates restart with change in direct input stiffness
                     matrices.

                       2x
K2XPP         DBM    [K  ] - Direct input stiffness matrix excluding
                       pp    hydroelastic boundary stiffness matrix.

                       4
K4AA          DBM    [K  ] - Partition of structural damping matrix.
                       aa

                       4
K4FF          DBM    [K  ] - Partition of structural damping matrix.
                       ff

                       4
K4GG          DBM    [K  ] - Structural damping matrix generated by Structural
                       gg    Matrix Assembler.

K4MX          DBS    Structural damping matrix.

                       4
K4NN          DBM    [K  ] - Partition of structural damping matrix.
                       nn

KAA           DBM    [K  ] - A-set stiffness matrix.
                       aa
                      _
KAAB          DBM    [K  ] - Partition of stiffness matrix.
                       aa

                       b
KBFS          DBM    [K  ] - Partition of combination of elastic stiffness
                       fs    matrix and differential stiffness matrix.

KBFL          DBM    [K    ] - Hydroelastic boundary stiffness matrix.
                       b,fl

                       b
KBLL          DBM    [K  ] - Combination of elastic stiffness and differential
                       ll    stiffness used in static analysis with
                             differential stiffness.

                       b
KBSS          DBM    [K  ] - Partition of combination of stiffness matrix and
                       ss    differential stiffness matrix.

                       d
KDAA          DBM    [K  ] - Partition of differential stiffness matrix.
                       aa

                        d
KDAAM         DBM    -[K  ] - Differential stiffness matrix used in formulation
                        aa    of buckling problems (D-5).

KDAMP         PU     -1 for structural damping, +1 for viscous.

KDD           DBM    [K  ] - Stiffness matrix used in direct formulation of
                       dd    dynamics problems (D-7 through D-9).

KDEK2         P      Parameter indicating equivalence of KDD and K2DD.

KDEKA         P      Parameter indicating equivalence of KDD and KAA.

                       d
KDFF          DBM    [K  ] - Partition of differential stiffness matrix.
                       ff

                       d
KDFS          DBM    [K  ] - Partition of differential stiffness matrix.
                       fs

                       d
KDGG          DBM    [K  ] - Differential stiffness matrix prepared by
                       gg    Differential Stiffness Matrix Generator.

                       d
KDNN          DBM    [K  ] - Partition of differential stiffness matrix.
                       nn

                       d
KDSS          DBM    [K  ] - Partition of differential stiffness matrix.
                       ss

KE            PH     Flutter analysis method.

KEF           DBM    [K  ] - Partition of stiffness matrix.
                       ff

KFS           DBM    [K  ] - Partition of stiffness matrix.
                       fs

KGG           DBM    [K  ] - Stiffness matrix generated by Structural Matrix
                       gg    Assembler.

                       l
KGGL          DBM    [K  ] - Stiffness matrix for linear elements. Used only in
                       gg    the Piecewise Linear Analysis Rigid Format (D-6).

KGGLPG        P      Purge flag for KGGL matrix. If set to -1, it implies that
                     there are no linear elements in the structural model.
                     (D-6).

                       nl
KGGNL         DBM    [K  ] - Stiffness matrix for the nonlinear elements. Used
                       gg    in the Piecewise Linear Analysis Rigid Format only.

KGGSUM        DBM    Sum of KGGNL and KGGL. Used In the Piecewise Linear
                     Analysis Rigid Format only. (D-6).

                       x
KGGX          DBM    [K  ] - Stiffness matrix excluding general elements.
                       gg

                       xl
KGGXL         DBM    [K  ] - Stiffness matrix for linear elements (excluding
                       gg    general elements). Used in the Piecewise Linear
                             Rigid Format only. (D-6).

                       y
KGGY          DBM    [K  ] - Stiffness matrix of general elements.
                       gg

KHH           DBM    [K  ] - Stiffness matrix used in modal formulation of
                       hh    dynamics problems (D-10 through D-12).

KINDEX        PU     A parameter which specifies a single value of the harmonic
                     index using cyclic symmetry.

KLL           DBM    [K  ] - Stiffness matrix used in solution of problems in
                       ll    static analysis (D-1, D-2, D-4, D-5, D-6).

KLR           DBM    [K  ] - Partition of stiffness matrix.
                       lr

KMAX          PU     A parameter which specifies the maximum value of the
                     harmonic index using cyclic symmetry.

KMTX          DBS    Stiffness matrix.

KNN           DBM    [K  ] - Partition of stiffness matrix.
                       nn

KOA           DBM    [K  ] - Stiffness matrix partition.
                       oa

KOO           DBM    [K  ] - Partition of stiffness matrix.
                       oo

KRR           DBM    [K  ] - Partition of stiffness matrix.
                       rr

KSS           DBM    [K  ] - Partition of stiffness matrix.
                       ss

KXHH          DBM    Total modal stiffness matrix - h-set.
=PAGE=
L             P      Parameter value used by MATGPR to print L-set matrices.

LABEL         EM     DMAP location.

LABEL         IC     Defines third line of titles to be printed on each page of
                     printer output. Also used on plots.

LABEL         IC     Requests identification of grid points and/or elements on
                     structure plot.

LAMA          DBT    Real eigenvalues.

LAMS          DBS    Eigenvalue data from modal reduce operation.

LAMX          FMU    Edit or generate data block, LAMA.

LBLi          L      A label used in displacement approach rigid formats where i
                     represents one or more characters used to form unique
                     labels.

                       b                                  b
LBLL          DBM    [L  ] - Lower triangular factor of [K  ].
                       ll                                 ll

LEFT TICS     IC     Request for tic marks to be plotted on left hand edge of
                     frame for X-Y plots.

LFREQ         PU     Low frequency limit for modal formulation of dynamics
                     problems (D-10 through D-12).

LGPWG         L      Label used in conjunction with the Grid Point Weight
                     Generator.

LINE          IC     Number of data lines printed per page of printer output. It
                     should be set to 50 for 11 x 17 inch paper, and to 35 for 8
                     1/2 x 17 inch paper.

LIST          IA     Used to list the problem deck from UMF or copy the problem
                     deck from UMF onto NUMF and list it.

LLL           DBM    [L  ] - Lower triangular factor of [K  ].
                       ll                                 ll

LMODES        PU     Number of lowest modes for modal formulation of dynamics
                     problems (D-10 through D-12).

LMTX          DBS    Decomposition product of REDUCE operation.

LOAD          IB     Static load combination definition.

LOAD          IC     Selects static structural loading condition or heat power
                     and/or flux.

LOADC         IB     Defines loading conditions for static analysis using
                     substructuring.

LOAD$         M      Indicates restart with change in static load set request.

LOAP          DBS    Load set identification numbers for appended load vectors.

LODS          DBS    Load set identification numbers.

LOGARITHMIC   IC     Requests logarithmic scales for X-Y plots.

LOGPAPER      IC     Requests logarithmic paper for X-Y plots.

LOO           DBM    [L  ] - Lower triangular factor of [K  ].
                       oo                                 oo

LOOP1$        M      Indicates looping problem in modified restart. (PM-4.3.7.l)

LOOPBGN       L      Signifies the beginning of the Piecewise Linear Analysis
                     Rigid Format DMAP Loop. (D-6).

LOOPEND       L      Signifies the end of the Piecewise Linear Analysis Rigid
                     Format DMAP loop. (D-6).

LOOP$         M      Indicates looping problem in modified restart. (PM-4.3.7.1)

LOOPTOP       L      Top of rigid format loop.

LOWER TICS    IC     Request for tic marks to be plotted on bottom edge of frame
                     for X-Y plots.

LSING         L      Used if [K  ] is singular.
                               oo

LUSET         P      Order of USET.

LUSETA        P      Number of degrees of freedom in the pa displacement set.

LUSETD        P      Order of USETD.
=PAGE=
M             P      Parameter value used by MATGPR to print M-set matrices.

                       2
M2DD          DBM    [M  ] - Partition of direct input mass matrix.
                       dd

                       2d
M2DPP         DBM    [M  ] - Direct input mass matrix for all physical points
                       pp    from Bulk Data Deck.

                       2
M2PP          DBM    [M  ] - Direct input mass matrix for all physical points.
                       pp

M2PP          IC     Direct input mass matrix selection.

M2PP$         M      Indicates restart with change in direct input mass
                     matrices.

MAA           DBM    [M  ] - Partition of mass matrix.
                       aa

MACH          PU     Velocity divided by speed of sound.

MASS          IB     Eigenvector normalization option - used on EIGR card.

MAT1          IB     Material definition card for isotropic material.

MAT2          IB     Material definition card for anisotropic material.

MAT3          IB     Material definition card for orthotropic material.

MAT4          IB     Thermal material definition card for isotropic material.

MAT5          IB     Thermal material definition card for anisotropic material.

MATGPR        FMU    Utility module for printing matrices with Grid Point
                     Identification.

MATPOOL       DBT    Grid point oriented direct input matrix data pool, output
                     by Input File Processor and used by functional module
                     MTRXIN.

MATPRN        FMU    Utility module for printing matrices.

MATPRT        FMU    Utility module for printing matrices with geometric grid
                     points.

Matrix        PH     A seven word array; the first word is a GINO file number,
Control              and words 2 through 7 comprise a matrix trailer.
Block

Matrix Data   PH     A data block is classified as a matrix if and only if it is
Block                generated by one of the NASTRAN matrix packing routines,
                     PACK or BLDPK.

Matrix        PH     A factorization of a matrix K so that K = LU, where L is a
Decomposition        unit lower triangular matrix and U is an upper triangular
                     matrix.

MATS1         IB     Specifies table references for stress-dependent material
                     properties.

MATT1         IB     Specifies table references for temperature-dependent
                     isotropic material properties.

MATT2         IB     Specifies table references for temperature-dependent
                     anisotropic material properties.

MATT3         IB     Specifies table references for temperature-dependent
                     orthotropic material properties.

MATT4         IB     Specifies table references for temperature-dependent
                     isotropic, thermal material properties.

MATT5         IB     Specifies table references for temperature-dependent,
                     anisotropic, thermal material properties.

MAX           IB     Eigenvector normalization option - used on EIGR, EIGB, and
                     EIGC cards.

MAXIMUM       IC     Indicates scale for deformed structure plots.
DEFORMATION

MAXIT         PU     Limits maximum number of iterations in nonlinear heat
                     transfer analysis.

MAXLINES      IC     Maximum printer output line count - default value is 20000.

MCE1          FMS    Multipoint Constraint Eliminator - part 1.

MCE2          FMS    Multipoint Constraint Eliminator - part 2.

MDD           DBM    [M  ] - Mass matrix used in direct formulation of dynamics
                       dd    problems (D-7 through D-9).

MDEMA         P      Parameter indicating equivalence of MDD and MAA.

MDLCEAD       P      Used in printing rigid format error messages for modal
                     complex eigenvalue analysis (D-10).

MDLFRRD       P      Used in printing rigid format error messages for modal
                     frequency response (D-11).

MDLTRD        P      Used in printing rigid format error messages for modal
                     transient response (D-12).

MEF1          DBT    Modal element forces, Sort 1 for OFP.

MEF2          DBT    Modal element forces, Sort 2 for OFP.

MERGE         FMM    Matrix merge functional module.

MES1          DBT    Modal element stresses, Sort 1 for OFP.

MES2          DBT    Modal element stresses, Sort 2 for OFP.

METHOD        IC     Selects method for real eigenvalue analysis.

METHOD        IS     Identifies EIGR Bulk Data card.

METHOD$       M      Indicates restart with change in eigenvalue extraction
                     procedures.

MFF           DBM    [M  ] - Partition of mass matrix.
                       ff

MGG           DBM    [M  ] - Mass matrix generated by Structural Matrix
                       gg    Assembler.

MHH           DBM    [M  ] - Mass matrix used in modal formulation of dynamics
                       hh    problems (D-10 through D-12).

MI            DBM    [m] - Modal mass matrix.

MIND          P      Minimum diagonal term of [U  ].
                                                oo

MKAERO1       IB     Provides table of Mach numbers and reduced frequencies (k).

MKAERO2       IB     Provides list of Mach numbers (m) and reduced frequencies
                     (k).

MLL           DBM    [M  ] - Partition of mass matrix.
                       ll

MLR           DBM    [M  ] - Partition of mass matrix.
                       lr

MMTX          DBS    Mass matrix.

MNN           DBM    [M  ] - Partition of mass matrix.
                       nn

MOA           DBM    [M  ] - Partition of mass matrix.
                       oa

MODA          FMX    User dummy module.

MODACC        FMS    Mode Acceleration Output Reduction Module.

MODACC        PU     A parameter to use the mode acceleration method.

MODAL         IC     Requests structure plots of mode shapes.

MODAL         P      Indicates modal as opposed to direct formulation of
                     dynamics.

MODAL         IA     Selects rigid format for modal complex eigenvalue analysis.
COMPLEX
EIGENVALUES

MODAL         IA     Selects rigid format for modal frequency and random
FREQUENCY            response.
RESPONSE

MODAL         IA     Selects rigid format for modal transient response.
TRANSIENT
RESPONSE

MODB          FMX    User dummy module.

MODC          FMX    User dummy module.

MODCOM        NP     Defines an array for module communications.

MODEL         IC     Indicates model number of structure plotter.

MODES         IA     Selects rigid format for normal mode analysis.

MODES         IC     Duplicates output requests for eigenvalue problems.

MODES         IS     Modes output request.

MODES         P      Used in printing rigid format error messages for normal modes
                     analysis (D-3).

Modified      PH     Restarting (see Restart) a NASTRAN problem and redirecting
Restart              its solution by changing the rigid format and/or selected
                     input data.

Module        PH     A logical group of subroutines which performs a defined
                     function.

MOMAX         IB     Conical shell moment definition card.

MOMENT        IB     Static moment load definition (vector).

MOMENT1       IB     Static moment load definition (magnitude and two grid
                     points).

MOMENT2       IB     Static moment load definition (magnitude and four grid
                     points).

MOO           DBM    [M  ] - Partition of mass matrix.
                       oo

MPC           IB     Multipoint constraint definition.

MPC           IC     Selects set of multipoint constraints for structural
                     displacement or heat transfer boundary temperature
                     relationships.

MPC$          M      Indicates restart with change in multipoint constraints.

MPCADD        IB     Multipoint constraint set definition.

MPCAX         IB     Conical shell multipoint constraint definition.

MPCFORCES     IC     Requests multipoint forces of constraint at a set of points
                     in Rigid Formats D-1, D-2, D-3, D-14, D-15.

MPCF1         P      No multipoint constraints.

MPCF2         P      No change in multipoint constraints for loop.

MPCS          IB     Specifies multipoint constraints for substructuring.

MPHIPA1       DBT    Eigenvectors, PA-set, SORT1.

MPHIPA2       DBT    Eigenvectors, PA-set, SORT2.

MPL           PH     Module properties list. The MPL defines each DMAP module's
                     name, the number of input, output, and scratch files
                     required, and the parameter list. It is used by the preface
                     module XGPI to generate the OSCAR.

MPT           DBT    Material Properties Table - output by Input File Processor.

MPY           M      Parameter constant used in executive module PARAM.

MPYAD         FMM    Performs multiply-add matrix operation.

MQP1          DBT    Constraint forces, PA-set, SORT1.

MQP2          DBT    Constraint forces, PA-set, SORT2.

MR            DBM    [M ] - Rigid body mass matrix.
                       r

MREDUCE       IS     Real modal reduction request.

MRR           DBM    [M  ] - Partition of mass matrix.
                       rr

MTRXIN        FMS    Selects direct input matrices for current loop in dynamics
                     problems (D-7 through D-12).

MX            IC     Indicates negative x-axis direction for structure plot.

MXHH          DBM    Total modal mass matrix - h-set.

MY            IC     Indicates negative y-axis direction for structure plot.

MZ            IC     Indicates negative z-axis direction for structure plot.
=PAGE=
N             M      Used in parameter section of DMAP statement. Indicates that
                     parameter may not be given an initial value with a PARAM
                     bulk data card.

N             P      Parameter value used by MATGPR to print N-set matrices.

NAME          IS     Specifies Phase 1 basic substructure name or names the
                     resulting substructure in Phase 2.

NASTPLT       IC     Requests NASTRAN general purpose plotter.

NASTRAN       M      Acronym for NAsa STRuctural ANalysis program.

NASTRAN Data  PH     The composite deck consisting of the Executive Control
Deck                 Deck, the Case Control Deck, the Substructure Control Deck,
                     and the Bulk Data Deck. This deck, when preceded by any
                     necessary operating system control cards, constitutes the
                     complete card input for a NASTRAN run (PM-5).

NCHECK        IC     Requests significant digits to indicate numerical accuracy
                     of element stress and force computations.

NDET          P      Power of 10 used to scale parameter DET.

NE            P      Parameter value used by MATGPR to print out NE-set
                     matrices.

NEIGV         P      Number of real eigenvalues found.

NEVER         P      Set to +1 by a DMAP PARAM statement in the Piecewise Linear
                     Analysis Rigid Format (D-6).

New Problem   PH     See Problem Tape.
Tape

NJ            P      Number of degrees of freedom in the j displacement set.

NK            P      Number of degrees of freedom in the k displacement set.

NLFT          DBT    Nonlinear function table.

NLLOAD        IC     Requests nonlinear load output for transient problems.

NLOAD         PU     A parameter of static loading conditions using cyclic
                     symmetry.

NMAX          IS     Identifies number of lowest frequency modes for retained
                     modal coordinates.

NO            IA     Option used on CHKPNT card, Indicates that no checkpoint is
                     desired.

NOA           P      Indicates no constraints applied to structural model.

NOABFL        P      No fluid-structure interface in a hydroelastic problem.

NOB2PP        P      No direct input damping matrix.

NOBGG         P      No viscous damping matrix (D-7 through D-9).

NOCEAD        P      Used to skip CEAD module when not required.

NOCSTM        P      No Coordinate System Transformation Matrices.

NOD           P      No output request that is limited to independent degrees of
                     freedom.

NODJE         PU     Positive value selects D1JE and D2JE from INPUTT2.

NODLT         P      No Dynamic Loads Table.

NOEED         P      No Eigenvalue Extraction Data.

NOELMT        P      No elements are defined.

NOFL          P      No fluid-structure interface and no fluid gravity in a
                     hydroelastic problem.

NOFRL         P      No Frequency Response List.

NOFRY         P      Used by aeroelastic response for transient solution.

NOGENEL       P      No general elements.

NOGPDT        P      No Grid Point Definition Table.

NOGPST        P      No grid point singularity table.

NOGRAV        P      No gravity loads.

NOGUST        P      No gust input.

NOH           L      Used to skip modal output.

NOH           P      Used to skip modal output.

NOK2PP        P      No direct input stiffness matrices.

NOK4GG        P      No structural damping matrix.

NOKBFL        P      No fluid gravity or structural interface in a hydroelastic
                     problem.

NOL           P      No independent degrees of freedom.

NOLIN1        IB     Nonlinear transient dynamic load set definition card.

NOLIN2        IB     Nonlinear transient dynamic load set definition card.

NOLIN3        IB     Nonlinear transient dynamic load set definition card.

NOLIN4        IB     Nonlinear transient dynamic load set definition card.

NOLOOP$       M      Indicates restart of problem without DMAP loop. (PM-
                     4.3.7.l).

NOM2DPP       P      No direct input mass matrix from Bulk Data Deck.

NOM2PP        P      No direct input mass matrices.

NOMGG         P      If functional module SMA2 generates a zero mass matrix,
                     NOMGG is set to -1. Otherwise, it is set to +1.

NOMOD         P      Mode acceleration data recovery not requested.

NONCUP        P      Indicates diagonal MHH, BHH, and KHH allowing uncoupled
                     solution in TRD and FRRD.

NONE          IC     Override for output and bulk data deck echo requests.

NONLIFT       P      No nonlinear function table.

NONLINEAR     IC     Selects nonlinear load for transient problems.

NONLINEAR     IA     Selects rigid format for nonlinear static analysis using
STATIC HEAT          heat transfer.
TRANSFER
ANALYSIS

NONLSTR       P      No stress output request for nonlinear elements (D-6).

NOP           M      Parameter constant used in executive module PARAM.

NOP           P      No output request involving dependent degrees of freedom or
                     stresses.

NOPF          L      Skip load calculations in transient aeroelastic response.

NOPSDL        P      No Power Spectral Density List.

NORMAL MODES  IA     Selects rigid format for normal mode analysis.

NORMAL MODES  IA     Selects rigid format for normal modes analysis using cyclic
ANALYSIS WITH        symmetry.
CYCLIC
SYMMETRY

NORMAL MODES  IA     Selects rigid format for normal modes analysis with
WITH                 differential stiffness effects.
DIFFERENTIAL
STIFFNESS

NORN          P      No random requests.

NOSET         P      No dependent coordinates.

NOSIMP        P      No structural elements are defined.

NOSORT2       P      No request for output sorted by point number or element
                     number.

NOSR          P      No single-point constraints or free body supports.

NOT           M      Parameter constant used in utility module PARAM.

NOTFL         P      No Transfer Function List.

NOTRL         P      No Transient Response List.

NOUE          P      No extra points introduced for dynamic analysis.

NOUE1         L      No extra points.

NOXYCBD       P     -1 indicates no XY output requests.

NOXYOUT       L      No XY-output requests.

NOXYPL        P      No XY-plot requests.

NOXYPLTT      L      No XY-plot requests.

NPLALIM       P      Set by module PLA1 as the Piecewise Linear Analysis Rigid
                     Format DMAP loop counter. (D-6)

NPTP          M      New Problem Tape - a reserved NASTRAN physical file which
                     must be set up by you when used.

NSEGS         PU     A parameter of identical segments in the structural model
                     using cyclic symmetry.

NSIL          P      Order of SIL table.

NSIL1         P      Number of grid and scalar points.

NSKIP         P      Locate current boundary conditions in Case Control.

NT            PU     A parameter to limit the cumulative number of iterations
                     for the static analysis with differential stiffness loops.

NUMF          IA     Used to add problem deck to NUMF, list it, and punch UMF
                     card.

NUMF          M      New User Master File - used only when operating NASTRAN as
                     a user master file editor. (See UMFEDIT). A reserved
                     NASTRAN physical file which must be set up by you when
                     used.

NVECTS        P      Number of eigenvectors found.
=PAGE=
O             P      Parameter value used by MATGPR to print O-set matrices.

OBEF1         DBT    Element force output table (D-5).

OBES1         DBT    Element stress output table (D-5).

OBQG1         DBT    Forces of single point constraint output table (D-5).

OCEIGS        DBT    Complex eigenvalue summary table (D-7, D-10).

OCPHIP        DBT    Complex eigenvector output table (D-7, D-10).

OCPHIPA       DBT    Complex eigenvector output table, aeroelastic.

OEF1          DBT    Element force output table (D-1, D-2, D-4, D-5, D-6).

OEF2          DBT    Element force output table - SORT2 (D-9, D-12).

OEFB1         DBT    Element force output table (D-4).

OEFC1         DBT    Element force output table - complex (D-7, D-8, D-10, D-
                     11).

OEFC2         DBT    Element force output table - complex - SORT2 (D-8, D-11).

OEIGS         DBT    Real eigenvalue summary output table (D-3, D-5).

OES1          DBT    Element stress output table (D-1, D-2, D-4, D-5, D-6).

OES2          DBT    Element stress output table - SORT2 (D-9, D-12).

OESB1         DBT    Element stress output table (D-4).

OESC1         DBT    Element stress output table - complex (D-7, D-8, D-10, D-
                     11).

OESC2         DBT    Element stress output table - complex - SORT2 (D-8, D-11).

OFP           FMS    Output File Processor.

OFREQ         IC     Output Frequency set.

OFREQUENCY    IC     Selects a set of frequencies to be used for output
                     requests in frequency response problems (default is all
                     frequencies) or flutter velocities.

OGPST         DBT    Grid point singularity output table.

OGPWG         DBT    Grid point weight generator output table.

OLDBOUND      IS     Flag to identify rerunning problem with previously defined
                     boundary set.

OLDMODES      IS     Flag to identify rerunning problem with previously
                     computed modal data.

Old Problem   PH     See Problem Tape.
Tape

OLOAD         IC     Request for output of external load vector.

OLOAD         IS     Applied load output request.

OMIT          IB     Omitted coordinate definition card.

OMIT          P      Indicates no omitted coordinates.

OMIT1         IB     Omitted coordinate definition card.

OMITAX        IB     Omitted coordinate definition card for conical shell
                     problems.

ONLES         DBT    Output table for nonlinear element stresses (D-6).

Open Core     PH     A contiguous block of working storage defined by a labeled
                     common block, whose length is a variable determined by the
                     NASTRAN executive routine CORSZ.

OPG1          DBT    Static load output table (D-1, D-2, D-4, D-5, D-6).

OPHID         DBT    Output table for complex eigenvectors - solution set (D-7).

OPHIG         DBT    Eigenvector output table (D-3, D-5).

OPHIH         DBT    Output table for complex eigenvectors - solution set (D-
                     10).

OPNL1         DBT    Output table for nonlinear loads - solution set, SORT1 (D-
                     9, D-12).

OPNL2         DBT    Output table for nonlinear loads - solution set, SORT2 (D-
                     9, D-12).

OPP1          DBT    Dynamic load output table (D-9, D-12).

OPP1          DBT    Aerodynamic transient load output table, sort 1.

OPP2          DBT    Dynamic load output table - SORT2 (D-9, D-12).

OPPC1         DBT    Dynamic load output table - SORT1, complex (D-8, D-11).

OPPC2         DBT    Dynamic load output table - SORT2, complex (D-8, D-11).

OPT           PU     Controls the type of multipoint constraint output.

OPTIONS       IS     Defines matrix types.

OPTP          M      Old Problem Tape - a reserved NASTRAN physical file which
                     must be set up by you when used.

OQBG1         DBT    Forces of single-point constraint output table (D-4).

OQG1          DBT    Single-point constraint force output table (D-1, D-2, D-4,
                     D-5, D-6).

OQP1          DBT    Single-point constraint force output table SORT1 (D-9, D-
                     12).

OQP2          DBT    Single-point constraint force output table SORT2 (D-9, D-
                     12).

OQPC1         DBT    Single-point constraint force output table - complex, SORT1
                     (D-7, D-8, D-10, D-11).

OQPC2         DBT    Single-point constraint force output table - complex, SORT2
                     (D-7, D-8, D-10, D-11).

OQPCA1        DBT    Complex constraint force output table, aeroelastic.

OR            M      Parameter constant used in executive module PARAM.

ORIGIN        IC     Locates origin for structure plot.

ORTHOGRAPHIC  IC     Specifies orthographic projection for structure plot.

OSCAR         PM     Operation sequence control array. Executive table residing
                     on the Data Pool File which contains the sequence of
                     operations to be executed for a problem solution. The OSCAR
                     is an expansion of a DMAP sequence, either input by you
                     or extracted from a rigid format, in internal format.

OTIME         IC     Selects a set of times to be used for output requests in
                     transient analysis problems (default is all times).

OUBGV1        DBT    Displacement vector output table (D-4).

OUDV1         DBT    Displacement vector output table - solution set, SORT1 (D-
                     9).

OUDV2         DBT    Displacement vector output table - solution set, SORT2 (D-
                     9).

OUDVC1        DBT    Displacement vector output table - solution set, SORT1,
                     complex (D-8, D-11).

OUDVC2        DBT    Displacement vector output table - solution set, SORT2,
                     complex (D-8, D-11).

OUGV1         DBT    Displacement output table (D-1, D-2, D-4, D-5, D-6).

OUHV1         DBT    Displacement vector output table - solution set, SORT1 (D-
                     12).

OUHV2         DBT    Displacement vector output table - solution set, SORT2 (D-
                     12).

OUHVC1        DBT    Displacement vector output table - solution set, SORT1,
                     complex (D-11).

OUHVC2        DBT    Displacement vector output table - solution set, SORT2,
                     complex (D-11).

OUPV1         DBT    Displacement vector output table - SORT1 (D-9, D-12).

OUPV2         DBT    Displacement vector output table - SORT2 (D-9, D-12).

OUPVC1        DBT    Displacement vector output table - complex, SORT1 (D-8, D-
                     11).

OUPVC2        DBT    Displacement vector output table - complex, SORT2 (D-8, D-
                     11).

OUTPUT        FMX    Auxiliary output file processor.

OUTPUT        IC     Marks beginning of printer output request packet -
                     optional.

OUTPUT        IS     Specifies optional output results.

Output Data   PM     A data block output from a module. May be output from one
Block                and only one module. Having been output, it may be used as
                     an input data block as many times as necessary.

OUTPUT1       FMU    Writes data blocks on GINO-written user tapes.

OUTPUT2       FMU    Writes data blocks on FORTRAN-written user tapes.

OUTPUT3       FMU    Punches matrices on DMI cards.

OUTPUT4       FMX    Auxiliary output file processor.
=PAGE=
P             P      Parameter value used in MATGPR to print P-set matrices.

P             PH     Flutter analysis method.

Packed Format PH     A matrix is said to be in packed format if only the nonzero
                     elements of the matrix are written.

PAERO1        IB     Aerodynamic Panel Property.

PAERO2        IB     Properties of aerodynamic bodies.

PAERO3        IB     Defines Mach Box geometries.

PAERO4        IB     Properties of strips (strip theory).

PAERO5        IB     Properties of strips (piston theory).

PAPER SIZE    IC     Selects paper size for structure plots using table
                     plotters.

PAPP          DBS    Appended load vectors.

PARAM         FMU    Manipulates parameter values.

PARAM         IB     Parameter definition card.

Parameter     PH     A FORTRAN variable communicated to a DMAP module by the
                     NASTRAN Executive System through blank common. A
                     parameter's position in the DMAP calling sequence to a
                     module corresponds to the position of the parameter in
                     blank common at module execution time.

PARAML        FMU    Selects parameters from a user input matrix or table.

PARAMR        FMU    Performs specified operations on real or complex
                     parameters.

PARTN         FMM    Matrix partitioning functional module.

PARTVEC       FMX    User dummy module.

PASSWORD      IS     SOF file protection.

PBAR          IB     Bar property definition card.

PBL           DBM    A scalar multiple of the PL load vector. Used only in the
                     Differential Stiffness Rigid Format (D-4).

PBS           DBM    A scalar multiple of the PL load vector. Used only in the
                     Differential Stiffness Rigid Format (D-4).

PCDB          DBT    Plot control data block (table for use with structure
                     plotter functional module PLTSET).

PCONEAX       IB     Conical shell element property definition card.

PCPHIPA       DBT    Complex displacement plot file.

PDAMP         IB     Scalar damper property definition card.

PDF           DBM    Dynamic load matrix for frequency analysis.

PDT           DBM    Linear dynamic load matrix for transient analysis.

PDUMi         IB     Property definition card for dummy elements 1 through 9.

PELAS         IB     Scalar elastic property definition card.

PEN           IC     Selects pen size for structure plots using table plotters.

PENSIZE       IC     Selects pen size for X-Y plots using table plotters.

PERSPECTIVE   IC     Specifies perspective projection for structure plots.

PFILE         P      Parameter used by PLOT module.

PG            DBM    Incremental load vector used in Piecewise Linear Analysis
                     (D-6).

PG            DBM    Statics load vector generated by SSG1.

PG1           DBM    Static load vector for Piecewise Linear Analysis (D-6).

PGG           DBM    Appended static load vector (D-1, D-2).

PGV1          DBM    Matrix of successive sums of incremental load vectors used
                     only in Piecewise Linear Analysis Rigid Format (D-6).

PHASE         IC     Requests magnitude and phase form of complex quantities.

Phase 1       PH     An operation to create matrices and load vectors for
                     substructuring analysis.

Phase 2       PH     An operation to combine and reduce matrices and load
                     vectors for substructuring analysis.

Phase 3       PH     An operation to recover detailed data reduction for
                     substructuring analysis.

PHBDY         IB     Boundary element property definition card for heat transfer
                     analysis.

PHF           DBM    Total frequency response loads, modal.

PHFI          DBM    Non-gust frequency response loads, modal.

PHIA          DBM    [í ] - Real eigenvectors - solution set.
                       a

PHIAH         DBM    Eigenvectors, A-set.

PHID          DBM    [í ]  - Complex eigenvectors - solution set, direct
                       a     formulation.

PHIDH         DBM    [í  ] - Transformation matrix between modal and physical
                       dh    coordinates.

PHIG          DBM    [í ]  - Real eigenvectors.
                       g

PHIH          DBM    [í ]  - Complex eigenvectors - solution set, modal
                       h     formulation.

PHIHL         DBM    Appended complex mode shapes - h-set.

PHIK          DBM    Eigenvectors, aerodynamic box points.

PHIL          DBS    Left side eigenvector matrix from unsymmetric CREDUCE
                     operation.

PHIP          DBM    Eigenvectors, P-set.

PHIPA         DBM    Eigenvectors, PA-set.

PHIPS         DBM    Eigenvectors, PS-set.

PHIS          DBS    Eigenvector matrix.

Physical      PH     Grid points and extra scalar points introduced for dynamic
Points               analysis.

PIECEWISE     IA     Selects rigid format for piecewise linear analysis.
LINEAR

Pivot Point   PH     The first word of each record of the GPCT and ECPT data
                     blocks is called the pivot point.

PJUMP         P      Used to skip deformed plots.

PK            PH     Flutter analysis method.

PKF           DBML   Forces on aerodynamic boxes, as a function of frequency.

PL            DBM    {P } - Partition of load vector.
                       l

PLA           P      Used in printing rigid format error messages for Piecewise
                     Linear Analysis (D-6).

PLA1          FMS    Piecewise Linear Analysis - phase 1.

PLA2          FMS    Piecewise Linear Analysis - phase 2.

PLA3          FMS    Piecewise Linear Analysis - phase 3.

PLA4          FMS    Piecewise Linear Analysis - phase 4.

PLACOUNT      P      Loop counter in Piecewise Linear Analysis (D-6).

PLALBL2A      L      Used in the Piecewise Linear Analysis Rigid Format only (D-
                     6).

PLALBL3       L      Used in the Piecewise Linear Analysis Rigid Format only (D-
                     6).

PLALBL4       L      Used in the Piecewise Linear Analysis Rigid Format only (D-
                     6).

PLCOEFFICIENT IC     Selects the coefficient set for Piecewise Linear Analysis
                     problems.

PLFACT        IB     Piecewise Linear Analysis factor definition card.

                       i
PLI           DBM    {P } - Partition of inertia relief load vector.
                       l

PLIMIT        IB     Property Optimization limits.

PLOAD         IB     Pressure load definition (D-1, D-2, D-4, D-5, D-6).

PLOAD2        IB     Element pressure loading for two-dimensional elements (D-1,
                     D-2, D-4, D-5, D-6).

PLOT          FMS    Structure plot generator.

PLOT          IC     Execution card for structure plotter.

PLOT          IS     Phase 2 undeformed plot request.

PLOT$         M      Indicates restart with a structure plot request.

Plot Tapes    PH     Magnetic tapes containing NASTRAN generated data to drive
                     offline plotters. PLT1 is the name of the BCD plot tape and
                     PLT2 is the name of the binary plot tape.

PLOTEL        IB     Plot element definition card used to define convenient
                     reference lines in structure plots.

PLOTTER       IC     Used to select one of several available plotters for
                     structure plotter.

PLOTX1        DBT    Messages from plot module concerning action taken by the
                     structure plotter in processing undeformed structure
                     plots.

PLOTX2        DBT    Messages from plot module concerning action taken by the
                     structure plotter in processing deformed structure plots.

PLOTX3        DBT    Deformed plot messages for aeroelastic.

PLSETNO       P      Set number on a PLFACT bulk data card chosen by you in
                     your case control deck. Used only in Piecewise Linear
                     Analysis (D-6).

PLT1          M      A reserved NASTRAN physical file which must be set up by
                     you when used - see Plot Tapes.

PLT2          M      A reserved NASTRAN physical file which must be set up by
                     you when used - see Plot Tapes.

PLTFLG        P      Parameter used by PLOT module.

PLTMRG        FMSS   Substructure plot set data merge.

PLTPAR        DBT    Plot control table.

PLTPARA       DBT    Plot control table PLTPAR, with aeroelastic data.

PLTS          DBS    Plot sets and other data required for Phase 2 plotting.

PLTSET        FMS    Plot set definition processor.

PLTSETA       DBT    Set definitions for aerodynamic plots.

PLTSETX       DBT    Error messages for plot sets.

PLTTRAN       FMS    Prepares data blocks for acoustic analysis plots.

PLTTRAN       FMS    Transforms grid point definition tables for scalar points
                     into a format for plotting.

PMASS         IB     Scalar mass property definition card.

                       n
PNLD          DBM    {P } - Nonlinear loads in direct transient problem.
                       d

                       n
PNLH          DBM    {P } - Nonlinear loads in modal transient problem.
                       h

PO            DBM    {P } - Partition of load vector.
                       o

POAP          DBS    Appended load vectors on omitted points.

                       i
POI           DBM    {P } - Partition of inertia relief load vector.
                       o

POINT         IB     Eigenvalue analysis normalization option for eigenvectors -
                     see EIGR, EIGC, EIGB cards.

POINTAX       IB     Axisymmetric Point.

POOL          M      Pool file used by file allocator.

POSITION      IS     Specifies initial position of input file.

POUT$         M      Indicates restart with a printer output request.

POVE          DBS    Load vectors on points omitted during matrix reduction.

PPF           DBM    Dynamic loads for frequency response.

PPHIG         DBM    Eigenvector components used to plot deformed shape. (D-3,
                     D-5).

PPT           DBM    Linear dynamic loads for transient analysis.

PQDMEM        IB     Quadrilateral membrane element property definition card.

PQDMEM1       IB     Isoparametric quadrilateral membrane element property
                     definition card.

PQDMEM2       IB     Quadrilateral membrane element property definition card.

PQDPLT        IB     Quadrilateral bending element property definition card.

PQUAD1        IB     General quadrilateral element property definition card.

PQUAD2        IB     Homogeneous quadrilateral element property definition card.

PREC          P      Precision of computer. CDC = 1; DEC VAX = 2; IBM = 2;
                     UNIVAC = 2.

PRECHK        EM     Predefined automated checkpoint.

Preface       PH     Executive routines which are executed prior to the
                     execution of the first module in a DMAP sequence. The
                     Preface consists of the executive routines necessary to
                     generate initial NASTRAN operational data and tables. The
                     primary Preface routines are GNFIAT, XCSA, IFP1, XSORT,
                     IFP, IFP3, and XGPI.

PREFIX        IS     Prefix to rename equivalenced lower level substructures.

PRESAX        IB     Defines static pressure loading for the conical shell
                     element.

PRESPT        IB     Defines a point in a hydroelastic model for output
                     purposes.

PRESSURE      IC     Request for output of pressure and displacement vector or
                     eigenvector for a hydroelastic problem.

PRINT         IA     Used to list all problem decks from UMF and Summary Table
                     of Contents.

PRINT         IS     Stores modal or solution data and prints data requested.

PRINT         PU     Controls printing of flutter summary.

Problem Tape  PH     A magnetic tape containing data necessary for NASTRAN
                     problem restarts. A tape being generated is designated as
                     the New Problem Tape (NPTP) and its content is largely
                     controlled by the DMAP instruction CHKPNT. This same tape
                     when used as input to a subsequent NASTRAN restart is
                     designated as the Old Problem Tape (OPTP).

PROD          IB     Rod property definition card.

PROJECTION    IC     Separation of observer and projection plane for structure
PLANE                plots.
SEPARATION

PRTMSG        FMS    Message generator.

PRTPARM       FMU    Prints DMAP diagnostic messages and parameter values.

PS            DBM    {P } - Partition of static load vector.
                       s

PSDF          DBM    Power Spectral Density Function table.

PSDF          IC     Request for output of Power Spectral Density Function in
                     Random Analysis (D-9, D-11).

PSDL          DBT    Power Spectral Density List.

Pseudo        PH     Restarting (see Restart) a NASTRAN problem and redirecting
Modified             its solution but only affecting output data.
Restart

PSF           DBM    Partition of load vector for transient analysis.

PSHEAR        IB     Shear panel property definition card.

PST           DBM    Partition of linear load vector for transient analysis.

PTITLE        IC     Structure plot frame title.

PTORDRG       IB     Toroidal ring property definition card.

PRTBSC        IB     Basic bending triangular element property definition card.

PTRIA1        IB     General triangular element property definition card.

PTRIA2        IB     Homogeneous triangular element property definition card.

PTRIM6        IB     Linear strain triangular membrane property.

PTRMEM        IB     Triangular membrane element property definition card.

PTRPLT        IB     Triangular bending element property definition card.

PTRPLT1       IB     Triangular plate property.

PTRSHL        IB     Higher order triangular shell element property.

PTUBE         IB     Tube property definition card.

PTWIST        IB     Twist panel property definition card.

PUBGV1        DBT    Displacement vector components used to plot deformed shape
                     (D-4, D-5).

PUGV          DBT    Displacement vector components used to plot deformed shape
                     (D-1, D-2).

PUGV1         DBT    Displacement components used to plot deformed shape (D-6).

PUNCH         IA     Used to punch the problem deck from UMF or copy the problem
                     deck from UMF onto NUMF and punch it.

PUNCH         IC     Output medium request (PRINT or PUNCH).

PUNPRT        IA     Used to punch and print the problem deck from UMF or copy
                     the problem deck from UMF onto NUMF and punch and print it.

PURGE         EM     DMAP statement which causes conditional purging of data
                     blocks.

Purge         PH     A data block is said to be purged when it is flagged in the
                     FIAT so that it will not be allocated to a physical file
                     and so that modules attempting to access it will be
                     signaled.

PUVPAT        DBT    Displacement vector used for plots, PA-set for aeroelastic.

PVEC          DBS    Load vectors.

PVISC         IB     Viscous element property definition card.

PVT           PH     Parameter value table. The PVT contains BCD names and
                     values of all parameters input by means of PARAM bulk data
                     cards. It is generated by the preface module IFP and is
                     written on the Problem Tape.

P1            PU     INPUTT2 rewind option.

P2            PU     INPUTT2 unit number.

P3            PU     INPUTT2 tape id.
=PAGE=
Q             PU     Parameter which defines the dynamic pressure.

QBDY1         IB     Defines uniform heat flux into HBDY elements.

QBDY2         IB     Defines grid point heat flux into HBDY elements.

QBG           DBM    Single point forces of constraint in the Differential
                     Stiffness Rigid Format (D-4).

QDMEM         IC     Requests structure plot for all QDMEM elements.

QDMEM1        IC     Requests structure plot for all QDMEM1 elements.

QDMEM2        IC     Requests structure plot for all QDMEM2 elements.

QDPLT         IC     Requests structure plot for.all QDPLT elements.

QG            DBM    Constraint forces for all grid points.

QHBDY         IB     Defines thermal load for steady-state heat conduction.

QHHL          DBML   Aerodynamic matrix list - h-set.

QHJL          DBML   Aerodynamic matrix for gust calculations.

QJHL          DBML   Aerodynamic transformation matrix between h and j sets.

QKHL          DBML   Aerodynamic matrix for aerodynamic force data recovery.

QP            DBM    Constraint forces for all physical points.

QPA           DBM    Constraint forces, PA-set.

QPAC          DBM    Constraint forces, complex, PA-set.

QPC           DBM    Complex single point forces of constraint for all physical
                     points.

QPP2          DBT    Aerodynamic transient load output, sort 2.

QR            DBM    {q } - Determinant support forces.
                       r

QS            DBM    {q } - Single-point constraint forces.
                       s

QUAD1         IC     Requests structure plot for all QUAD1 elements.

QUAD2         IC     Requests structure plot for all QUAD2 elements.

QVEC          DBS    Reaction force vectors.

QVECT         IB     Defines thermal vector flux from distant source.

QVOL          IB     Defines volume heat generation.
=PAGE=
R             P      Parameter value used by MATGPR to print R-set matrices.

R1            IC     Request for X-Y plot of the first rotational component
                     (UM-4.3).

R1IP          IC     Request for X-Y plot of the first rotational component -
                     imaginary and phase angle (UM-4.3).

R1RM          IC     Request for X-Y plot of the first rotational component -
                     real and magnitude (UM-4.3).

R2            IC     Request for X-Y plot of the second rotational component
                     (UM-4.3).

R2IP          IC     Request for X-Y plot of the second rotational component -
                     imaginary and phase angle (UM-4.3).

R2RM          IC     Request for X-Y plot of the second rotational component -
                     real and magnitude (UM-4.3).

R3            IC     Request for X-Y plot of the third rotational component
                     (UM-4.3).

R3IP          IC     Request for X-Y plot of the third rotational component -
                     imaginary and phase angle (UM-4.3).

R3RM          IC     Request for X-Y plot of the third rotational component -
                     real and magnitude (UM-4.3).

RADLIN        P      Controls linearization of radiation effects in transient
                     heat transfer analysis.

RADLST        IB     List of radiation areas.

RADMTX        IB     Radiation exchange coefficients.

RANDOM        IC     Selects the RANDPS and RANDT cards to be used in random
                     analysis.

RANDOM        EMS    Random response solution generator.

RANDPS        IB     Power spectral density specification.

RANDT1        IB     Autocorrelation function time lag.

RANDT2        IB     Autocorrelation function time lag.

RANGE         IS     Identifies frequency range for real or complex retained
                     modal coordinates.

RBMG1         FMS    Rigid body matrix generator - part 1.

RBMG2         FMS    Rigid body matrix generator - part 2.

RBMG3         FMS    Rigid body matrix generator - part 3.

RBMG4         FMS    Rigid body matrix generator - part 4.

RCOVR         FMSS   Recover Phase 2 substructure results.

RCOVR3        FMSS   Recover substructure results for Phase 3.

REACT         P      Flag for rigid body mode calculations.

READ          FMS    Real Eigenvalue Analysis - Displacement.

REAL          IC     Requests real and imaginary form of complex quantities.

REAL          IA     Selects rigid format for normal mode analysis.
EIGENVALUES

RECOVER       IS     Phase 2 solution data recovery or Phase 1, 2 modal
                     reduction request.

REDUCE        FMSS   Reduction of substructure degrees of freedom.

REDUCE        IS     Phase 2 reduction to retained degrees of freedom request.

REEL          IA     Term appearing on the checkpoint dictionary cards
                     indicating the physical reel on which a data block appears.

Reentry Point PH     The point in the DMAP sequence at which a problem
                     terminated and hence the point at which it can be restarted
                     (see Restart).

REGION        IC     Specifies portion of frame to be used for structure plot.

REIG          P      Parameter used in SDR2 to indicate Normal Mode Analysis (D-
                     3).

RELES         IB     Specifies grid point degrees of freedom to be disconnected
                     - overrides CONCT and automatic connectivities using
                     substructuring.

REMOVE        IA     Used to copy problem decks from UMF onto NUMF up to pid
                     and skip over problem pid.

REPCASE       IC     Allows another output request for the previous subcase (D-
                     1, D-2).

REPEAT        P      Controls looping in Static Analysis (D-1, D-2).

REPEATD       P      Controls looping in Static Analysis with Differential
                     Stiffness (D-4).

REPEATE       P      Controls looping in Complex Eigenvalue Analysis (D-7,  D-
                     10).

REPEATF       P      Controls looping in frequency Response Analysis (D-8,  D-
                     11).

REPEATT       P      Controls looping in Transient Response Analysis (D-9,  D-
                     12).

REPT          EM     DMAP statement to conditionally repeat a loop.

RESPONSE      IC     Request for X-Y plot of any response outputs from transient
                     or frequency response analysis (D-8, D-9, D-11, D-12).

RESTART       IA     First control card of checkpoint dictionary. Contains
                     identification of checkpoint tape.

Restart       PH     Initiating a NASTRAN problem solution at a place other than
                     its logical beginning by utilizing an Old Problem Tape
                     created during a previous run.

RESTORE       IB     Reloads the SOF from an external file.

RFORCE        IB     Rotational force definition card.

RFORCE$       M      Indicates restart with change in rotational force.

RD            DBM    Multipoint constraint equations.

RGRID         IB     Specifies grid point in the basic substructure to define
                     reference point for inertia relief shapes. Defaults to
                     origin of basic substructure coordinate system.

RIGHT TICS    IC     Request for tic marks to be plotted on right hand edge of
                     frame for X-Y plots.

Rigid Format  PH     A fixed prestored DMAP sequence and its associated restart
                     tables which perform a specific problem solution.

Rigid Format  PH     A type of restart (see Restart) in which the problem is
Switch               changed from one Rigid Format to another.

RINGAX        IB     Conical shell ring definition card.

RINGFL        IB     Hydroelastic axisymmetric point definition card.

RLOAD1        IB     Frequency response load set definition.

RLOAD2        IB     Frequency response load set definition.

RMG           FMH    Radiation matrix generator - generates [R  ].
                                                              gg

RNAME         IS     Specifies basic substructure to define reference point for
                     inertia.

ROD           IC     Requests structure plot for all ROD elements.

RP            DBM    Partitioning vector set D to A and E.

RSAVE         IS     Save REDUCE decomposition product, or indicates the
                     decomposition product of the interior point stiffness.

RUBLV         DBM    Residual vector - Differential Stiffness Rigid Format (D-
                     4).

RULV          DBM    Residual vector for independent degrees of freedom.

RUN           IS     Specifies run options.

RUOV          DBM    Residual vector for omitted degrees of freedom.

RXY           IC     Requests vector sum of X and Y deformation components for
                     structure plot.

RXYZ          IC     Requests vector sum of X, Y, and Z deformation components
                     for structure plot.

RXZ           IC     Requests vector sum of X and Z deformation components for
                     structure plot.

RYX           IC     Requests vector sum of Y and Z deformation components for
                     structure plot.
=PAGE=
S             P      Parameter value used by MATGPR to print S-set matrices.

SACCE         IC     Abbreviated form of SACCELERATION.

SACCELERATION IC     Output request for solution set acceleration vector. (UM-
                     2.3, 4.3)

SAVE          EM     DMAP statement which causes current value of parameter to
                     be saved.

SAVE          IS     Stores modal or solution data on SOF.

SAVE          M      Save data block for possible looping in DMAP sequence (see
                     FILE).

SAVEPLOT      IB     Requests plot data be saved in Phase 1.

SC            IC     Selects SC 4020 plotter.

SCALAR        FMU    Convert matrix element to parameter.

Scalar Point  PH     A point which is defined on an SPOINT, CELAS1, CELAS2,
                     CELAS3, CELAS4, CMASS1, CMASS2, CMASS3, CMASS4, CDAMP1,
                     CDAMP2, CDAMP3, or CDAMP4 bulk data card. A scalar point
                     has no geometrical coordinates and defines only one degree
                     of freedom of the model.

SCALE         IC     Selects scale for structure plot.

SCE1          FMS    Single-point Constraint Eliminator.

SDAMP         IC     Modal structural damping table selection.

SDAMP4        M      Indicates restart with change in modal damping.

SDAMPING      IC     Selects table which defines damping as a function of
                     frequency in modal formulation problems.

SDISP         IC     Abbreviated form of SDISPLACEMENT.

SDISPLACEMENT IC     Output request for solution set displacement vector.
                     (UM-2.3, 4.3)

SDR1          FMS    Stress Data Recovery - part 1.

SDR2          FMS    Stress Data Recovery - part 2.

SDR3          FMS    Stress Data Recovery - part 3.

SDRHT         FMS    Heat flux data recovery.

SEARCH        IS     Limits search for automatic connects.

SECTAX        IB     Defines conical shell sector for data recovery.

SEEMAT        FMU    Prints pictorial representation of matrix showing location
                     of nonzero elements.

SEM1          M      The NASTRAN Preface.

SEQEP         IB     Extra point resequencing.

SEQGP         IB     Grid or scalar point resequencing.

SET           IC     Definition of a set of elements, grid and/or scalar and/or
                     extra points, frequencies, or times to be used in selecting
                     output.

SET1          IB     Defines a set of structural grid points by a list.

SET2          IB     Defines a set of structural grid points by aerodynamic
                     macro elements.

SETVAL        FMU    Parameter value initiator.

SGEN          FMSS   Substructure table generator.

SHEAR         IC     Requests structure plot for all shear panel elements.

SIGMA         PU     Defines Stefan-Boltzmann constant in heat transfer
                     analysis.

SIL           DBT    Scalar Index List for all grid points and extra scalar
                     points introduced for dynamic analysis.

SILGA         DBT    Scalar Index List - Aerodynamic boxes only.

SINCON        PU     Controls the automatic stiffness matrix singularity
                     removal.

SINE          IC     Conical shell request for sine set boundary conditions.

SING          P      -1 if [K  ] is singular.
                             oo

SINGLE        P      No single-point constraints.

SKIP BETWEEN  IC     Request to insert blank frames on SC 4020 plotter for X-Y
FRAMES               plots.

SKJ           DBM    Integration matrix.

SKPMGG        P      Parameter used in statics to control execution of
                     functional module SMA2.

SKPPLT        L      Used to skip plot.

SLBDY         IB     Defines list of points on interface between axisymmetric
                     fluid and radial slots.

SLOAD         IB     Scalar point load definition.

SLT           DBT    Static Loads Table.

SMA1          FMS    Structural Matrix Assembler - phase 1 - generates stiffness
                                                                  4
                     matrix [K  ] and structural damping matrix [K  ].
                              gg                                  gg

SMA2          FMS    Structural Matrix Assembler - phase 2 - generates mass
                     matrix [M  ] and viscous damping matrix [B  ].
                              gg                               gg

SMA3          FMS    Structural Matrix Assembler - phase 3 - add general element
                     contributions to the stiffness matrix [K  ].
                                                             gg

SMP1          FMS    Structural Matrix Partitioner - part 1.

SMP2          FMS    Structural Matrix Partitioner - part 2.

SMPYAD        FMM    Performs multiply-add matrix operation for up to five
                     multiplications and one addition.

SOF           IB     Assigns physical files for storage of the SOF.

SOFI          FMSS   SOF into GINO matrix copier.

SOFIN         IS     Copies substructure items from an external file to the SOF.

SOFO          FMSS   SOF out from GINO matrix copier.

SOFOUT        IS     Copies substructure items from the SOF to an external file.

SOFPRINT      IS     Prints selected contents of the SOF.

SOFUT         FMSS   SOF utility module.

SOL           IA     Specifies which rigid format solution is to be used when
                     APP is DISPLACEMENT.

SOLN          DBS    Load factor data or eigenvalues used in a solution.

Solution      PH     Points used in the formulation of the general K system.
Points

SOLVE         FMM    Solves a set of linear algebraic equations.

SOLVE         IB     Requests substructure solution.

SORT          IS     Output sort order.

SORT1         IC     Output is sorted by frequency or time and then by external
                     ID.

SORT2         IC     Output is sorted by external ID and then by frequency or
                     time.

SORT3         M      Output is sorted by individual item or component and then
                     by frequency or time.

SPC           IB     Single-point constraint and enforced deformation
                     definition.

SPC           IC     Selects set of single-point constraints for structural
                     displacements or heat transfer boundary temperatures.

SPC$          M      Indicates restart with change in single-point constraint
                     set selection.

SPC1          IB     Single-point constraint definition.

SPCADD        IB     Single-point constraint set combination definition.

SPCAX         IB     Conical shell single-point constraint definition.

SPCF          IC     Abbreviated form of SPCFORCE.

SPCF          IS     Reaction force output request.

SPCFORCE      IC     Requests the single-point forces of constraint at a set of
                     points or the thermal power transmitted to a selected set
                     of points in heat transfer.

SPCS          IB     Specifies single point constraints for substructuring.

SPCS1         IB     Alternate specification of single point constraints for
                     substructuring.

SPCSD         IB     Specifies enforced displacements for single point
                     constraints for substructuring.

Spill         PH     Secondary storage devices are used because there is
                     insufficient main storage to perform a matrix calculation
                     or a data processing operation.

SPLINE        DBT    Splining Data Table.

SPLINE1       IB     Defines surface spline.

SPLINE2       IB     Defines beam spline.

SPLINE3       IB     User data to interpolate deflections at aerodynamic degrees
                     of freedom.

SPOINT        IB     Scalar point definition card.

SSG1          FMS    Static Solution Generator - part 1.

SSG2          FMS    Static Solution Generator - part 2.

SSG3          FMS    Static Solution Generator - part 3.

SSG4          FMS    Static Solution Generator - part 4.

SSGHT         FMH    Solution generator for nonlinear heat transfer analysis.

STATIC        IC     Requests deformed structure plot for problem in Static
                     Analysis.

STATIC        IA     Selects rigid format for static analysis using cyclic
ANALYSIS WITH        symmetry.
CYCLIC SYMMETRY

STATIC HEAT   IA     Selects rigid format for linear static analysis using heat
TRANSFER             transfer.
ANALYSIS

STATICS       IA     Selects statics rigid format for heat transfer or
                     structural analysis.

STATICS       P      Parameter used in SDR2 to indicate Static Analysis.

STEADY STATE  IA     Selects rigid format for nonlinear static heat transfer
                     analysis.

STEPS         IB     Frequency or time step output request for substructuring.

STEREOSCOPIC  IC     Requests stereoscopic projections for structure plot.

STRESS        IC     Requests the stresses in a set of structural elements or
                     the velocity components in a fluid element in acoustic
                     cavity analysis.

Structural    PM     One of the finite elements used to represent a part of a
Element              structure.

STST          NP     Defines the singularity tolerance in EMG.

SUBCASE       IC     Subcase definition.

SUBCASES      IB     Subcase output request.

SUBCOM        IC     This subcase is a linear combination of previous subcases.

SUBPH1        FMSS   Substructure, Phase 1.

SUBSEQ        IC     Specifies coefficients for SUBCOM subcases.

SUBSTRUCTURE  IB     Initiates the substructure control deck.

Substructure  PH     One of the data decks required to run automated multi-stage
Control Deck         substructuring. The deck begins with the SUBSTRUCTURE card
                     and terminates with the ENDSUBS card. Cards in this deck
                     cause the necessary alters to the Rigid Format DMAP.

SUBTITLE      IC     Output labeling data for printer output.

SUPAX         IB     Fictitious support for conical shell problem.

SUPORT        IB     Fictitious support definition card.

SVECTOR       IC     Request for output of eigenvectors in the solution set (D-
                     7, D-10) (UM-2.3, 4.3).

SVELO         IC     Abbreviated form of SVELOCITY.

SVELOCITY     IC     Requests velocity output for solution set. (UM-2.3, 4.3)

SWITCH        FMU    Interchange two data block names.

SYM           IC     Symmetry subcase delimiter card.

SYMBOLS       IC     Requests symbols at grid points on structure plot.

SYMCOM        IC     Assembly of symmetry subcase delimiter card.

SYMSEQ        IC     Assembly value of symmetry combination card.

SYMTRANSFORM  IB     Specifies symmetry transformation.
=PAGE=
T1            IC     Request for X-Y plot of the first translational component
                     (UM-4.3).

T1IP          IC     Request for X-Y plot of the first translational component -
                     imaginary and phase angle (UM-4.3).

T1RM          IC     Request for X-Y plot of the first translational component -
                     real and magnitude (UM-4.3).

T2            IC     Request for X-Y plot of the second translational component
                     (UM-4.3).

T2IP          IC     Request for X-Y plot of the second translational component
                     - imaginary and phase angle (UM-4.3).

T2RM          IC     Request for X-Y plot of the second translational component
                     - real and magnitude (UM-4.3).

T3            IC     Request for X-Y plot of the third translational component
                     (UM-4.3).

T3IP          IC     Request for X-Y plot of the third translational component -
                     imaginary and phase angle (UM-4.3).

T3RM          IC     Request for X-Y plot of the third translational component -
                     real and magnitude (UM-4.3).

TA1           FMS    Table Assembler.

TABDMP1       IB     Tabular structural damping function for modal formulation
                     (D-10, D-11, D-12).

Table Data    PH     A data block which is in tabular form rather than matrix
Block                form.

TABLED1       IB     Dynamic load tabular function (D-8, D-9, D-11, D-12).

TABLED2       IB     Dynamic load tabular function (D-8, D-9, D-11, D-12).

TABLED3       IB     Dynamic load tabular function (D-8, D-9, D-11, D-12).

TABLED4       IB     Dynamic load tabular function (D-8, D-9, D-11, D-12).

TABLEM1       IB     Material property tabular function.

TABLEM2       IB     Material property tabular function.

TABLEM3       IB     Material property tabular function.

TABLEM4       IB     Material property tabular function.

TABLES1       IB     Stress-dependent material tabular function for use in
                     Piecewise Linear Analysis (D-6).

TABPCH        FMU    Punches selected tables on DTI bulk data cards.

TABPRT        FMU    Formats selected table data blocks for printing.

TABPT         FMU    Table printer.

TABRNDG       IB     Table of Power Spectral Density for certain gusts.

TABRND1       IB     Tabular function for use in Random Analysis (D-8, D-11).

TABRND2       IB     Tabular function for use in Random Analysis (D-8, D-11).

TABRND3       IB     Tabular function for use in Random Analysis (D-8, D-11).

TABRND4       IB     Tabular function for use in Random Analysis (D-8, D-11).

TABS          P      Defines absolute reference temperature in heat transfer
                     analysis.

TALL EDGE     IC     Request for plotting all edge tic marks on upper half frame
TICS                 for X-Y plots.

TAPE          M      Write data block on physical tape (see FILE).

TCURVE        IC     Curve title for X-Y plot.

TEMP          IB     Grid temperature definition card.

TEMPAX        IB     Temperature definition for conical shell problem.

TEMPD         IB     Grid default temperature definition card.

TEMPERATURE   IC     Selects thermal field for determining both equivalent
                     static loads and material properties.

TEMPLD$       M      Indicates restart with change in thermal set for static
                     loading.

TEMPMT$       M      Indicates restart with change in thermal set for material
                     properties.

TEMPMX$       M      Indicates restart with change in thermal field with
                     thermally dependent material properties.

TEMP(LOAD)    IC     Selects thermal field to be used for determining equivalent
                     static loads.

TEMP(MAT)     IC     Selects thermal field to be used for determining structural
                     material properties or an estimate of the temperature
                     distribution for heat transfer iterations.

TEMPP1        IB     Plate element temperature definition card.

TEMPP2        IB     Plate element temperature definition card.

TEMPP3        IB     Plate element temperature definition card.

TEMPRB        IB     One-dimensional element temperature definition.

TF            IB     Dynamic transfer function definition.

TF$           M      Indicates restart with change in transfer function set
                     selection.

TFL           IC     Transfer function set selection.

TFPOOL        DBT    Transfer function pool.

THERMAL       IC     Request for output of temperature vector in thermal
                     analysis (UM-2.3).

THROUGH          IC     Forms strings of values within set declarations.

TIC           IB     Transient Initial Condition set definition card.

TIME          IA     User time estimate for problem. This card is required in
                     Executive Control Deck. Integer time value is in minutes.

TIMETEST      FMU    Provides NASTRAN system timing data.

TITLE         IC     Output labeling data for printer output.

TLEFT TICS    IC     Request for tic marks to be plotted on left hand edge of
                     top half frame for X-Y plot.

TLOAD1        IB     Transient load set definition card.

TLOAD2        IB     Transient load set definition card.

TOC           IA     Used to list all problem decks (Summary Table of Contents)
                     by UMF number from UMF.

TOL           DBT    Time output list.

TOL1          DBT    Reduced time output list, uses OTIME.

TOLERANCE     IS     Limits distance between automatically connected grids.

TRACKS        NP     Defines the format for the number of tracks required for
                     plot data.

Trailer       PH     A six word control block associated with a data block.

TRANRESP      P      Parameter used in SDR2 to indicate Transient Response
                     Analysis (D-9, D-12).

TRANS         IB     Specifies coordinate systems for substructure and grid
                     point transformation.

TRANSFORM     IS     Defines transformations for named component substructures.

TRANSIENT     IA     Selects rigid format for transient heat transfer analysis.

TRANSIENT     IA     Selects rigid format for linear transient analysis using
HEAT TRANSFER        heat transfer.
ANALYSIS

TRBSC         IC     Requests structure plot for all basic bending triangle
                     elements.

TRD           FMS    Transient Response - Displacement.

TRHT          FMH    Integrates dynamic equation for heat transfer analysis.

TRIA1         IC     Requests structure plot for all TRIA1 elements.

TRIA2         IC     Requests structure plot for all TRIA2 elements.

TRIGHT TICS   IC     Request for tic marks to be plotted on right hand edge of
                     top half frame for X-Y plots.

TRL           DBT    Transient Response List.

TRLG          FMH    Generates dynamic heat flux loads.

TRMEM         IC     Requests structure plot for all triangular membrane
                     elements.

TRNSP         FMM    Transpose functional module.

TRPLT         IC     Request structure plot for all TRPLT elements.

TSTART        P      CPU time at start of flutter loop.

TSTEP         IB     Transient time steps for integration and output.

TSTEP         IC     Transient time step set selection.

TSTEP$        M      Indicates restart with change in transient time step set
                     selection.

TUBE          IC     Requests structure plot for all TUBE elements.

TWIST         IC     Requests structure plot for all TWIST elements.

TYPE          IC     Indicates paper type for structure plots.
=PAGE=
UBGV          DBM    Displacement vector for all grid points (D-4).

                       b                                  b
UBLL          DBM    [U  ] - Upper triangular factor of [K  ].
                       ll                                 ll

UBLV          DBM    Displacement solution vector (D-4).

UBOOV         DBM    Scalar multiple of UOOV in Differential Stiffness Rigid
                     Format (D-4).

UDET          IB     Selects unsymmetric decomposition option for determinant
                     method of real eigenvalue analysis.

UDVIT         DBM    Displacement, velocity, and acceleration solution vectors
                     in a transient analysis problem - SORT1 (D-9).

UDV2T         DBM    Displacement, velocity, and acceleration solution vectors
                     in a transient analysis problem - SORT2 (D-9).

UDVF          DBM    Displacement solution vector in a frequency response
                     problem (D-8).

UDVT          DBM    Displacement, velocity, and acceleration solution vectors
                     in a transient analysis problem (D-9).

UEVF          DBM    Displacement vector for extra points in a frequency
                     response problem (D-11).

UEVT          DBM    Displacement vector for extra points in a transient
                     response problem (D-12).

UGV           DBM    Displacement vector for all grid points (D-1, D-2, D-4, D-
                     5).

UGV1          DBM    Successive sums of incremental displacement vectors.
                     Piecewise Linear Analysis Rigid Format only (D-6).

UHVF          DBM    Modal frequency response solution vectors (D-11).

UHVT          DBM    Modal transient response solution vectors (D-12).

UHVT1         DBM    Modal amplitudes for aeroelastic transient.

UIMPROVE      IS     Improved displacement request.

UINV          IB     Selects unsymmetric decomposition option for inverse power
                     method of eigenvalue analysis.

ULL           DBM    [U  ] - Upper triangular factor of [K  ].
                       ll                                 ll

ULV           DBM    Displacement solution vector in static analyses (D-1, D-2,
                     D-4, D-5).

UMERGE        FMM    Functional module to merge column matrices based on U-set.

UMF           IA     Used to copy UMF problem deck onto NUMF, list it and punch
                     UMF card.

UMF           M      User Master File, a reserved NASTRAN physical file which
                     must be set up by you when used.

UMFEDIT       IA     Requests User Master File operational mode of NASTRAN.

Unmodified    PN     Restarting (see Restart) a problem without changing any
Restart              data, other than output requests, of the previous run.

Unpool        PH     Remove data block from Pool Tape and place on a file for
                     use by a functional module.

UNSORT        IC     Requests unsorted echo of Bu1k Data Deck (ECHO=UNSORT).

UOO           DBM    [U  ] - Upper triangular factor of [K  ].
                       oo                                 oo

UOOV          DBM    Partition of displacement solution vector.

UPARTN        FMM    Functional module to partition matrices based on U-set.

UPPER TICS    IC     Request for tic marks to be plotted on upper edge of frame
                     for X-Y plot.

UPRT          DBS    Partitioning vector used in matrix reduction.

UPV           DBM    Transient solution sectors for all physical points.

UPVC          DBM    Frequency response solution vectors for all physical
                     points.

USERMODES     IS     Flag to indicate modal data have been input on bulk data.

USET          DBT    Displacement set definitions. (PM-1.7.3).

USETA         DBT    Displacement set definitions table - Aerodynamics.

USETD         DBT    Displacement set definitions including extra scalar points.

UVEC          DBS    Displacement vectors or eigenvectors.

UVT1          DBM    Displacements for aeroelastic transient.
=PAGE=
V             DBM    Partitioning vector for set F to O and A.

V             M      Used in parameter section of DMAP statement. Indicates that
                     parameter is variable and may be changed by module. If
                     changed value is to be used in subsequent DMAP instruction,
                     it must be saved (see SAVE).

VANTAGE POINT IC     Location of observer for structure plot.

VDR           FMS    Vector Data Recovery.

VDR           L      Used to skip to VDR module in flutter analysis.

VEC           FMU    Creates partitioning vector based on USET.

VECTOR        IC     Request for output of eigenvectors from real or complex
                     eigenvalue analysis (D-3, D-5, D-7, D-10).

VECTOR        IC     Requests displacements for a selected set of physical
                     points.

VELO          IC     Abbreviated form of VELOCITY.

VELO          IS     Velocity output request.

VELOCITY      IC     Output request statement for velocity vector. (UM-2.3,
                     4.2).

VFS           DBM    Partitioning vector for heat transfer analysis.

VIEW          IC     Rotation of object for structure plot.

VISC          IC     Request structure plot for all viscous damper element.

VPS           M      See XVPS.

VREF          PU     Velocity division factor.




W3            PU     Pivotal frequency for uniform structure damping in the
                     direct formulation of transient response problems (D-9).

W4            PU     Pivotal frequency for element structural damping in the
                     direct formulation of transient response problems (D-9).

WTMASS        PU     Weight to mass conversion factor used in SMA2 and GPWG.
                     Default value is 1.0.
=PAGE=
X             IC     Requests X vector for deformed structure plot.

XAXIS         IC     Request for drawing of X-axis for X-Y plot.

XBAXIS        IC     Request for drawing of X-axis on bottom half frame for X-Y
                     plot.

XBGRID LINES  IC     Request for drawing grid lines for X-axis on bottom half
                     frame for X-Y plot.

XCSA          EM     Executive Control Section Analysis. The preface module
                     which processes the Executive Control Deck and prepares the
                     control file on the New Problem Tape.

XDIVISIONS    IC     Request for division marking on X-axis.

XDMAP         EM     Controls the DMAP compiler options.

XGPI          EM     Executive General Problem Initialization. The preface
                     module whose principal function is to generate the OSCAR.
                     If the problem is a restart, XGPI initializes data blocks
                     and named common blocks for proper restart.

XGRID LINES   IC     Request for grid lines to be drawn on X-axis for X-Y plots.

XINTERCEPT    IC     Specifies intercept of Y-axis on X-axis.

XLOG          IC     Request for logarithmic scales in X-direction.

XMAX          IC     Do not plot points whose X value lies above this value.

XMIN          IC     Do not plot points whose X value lies below this value.

XPAPER        IC     Specifies length of paper in X-direction for table plotter.

XQHHL         P      Appended QHHL data parameter.

XSFA          EM     Executive Segment File Allocator - the administrative
                     manager of data blocks for NASTRAN.

XSORT         EM     Executive sort routine - the preface module which reads and
                     sorts the Bulk Data Deck and writes the sorted Bulk Data
                     Deck on the New Problem Tape.

XTAXIS        IC     Request for drawing of X-axis on top half frame.

XTGRID LINES  IC     Request for drawing of grid lines on top half frame.

XTITLE        IC     X-axis title for X-Y plots.

XVALUE PRINT  IC     Request to suppress labeling tic marks over the specified
SKIP                 interval.

XVPS          M      Variable Parameter Set Table. Executive table needed for
                     restart. (PM-2.4)

XY            IC     Requests X and Y vectors for deformed structure plot.

XYCDB         DBT    SORT3 output requests (XYPLOTTER, XYPRINTER, Random
                     Request).

XYOUT         IC     Request to generate X-Y plots.

XYOUT$        M      Indicates restart with an X-Y plot request.

XYPEAK        IC     Request to print the maximum and minimum values of the
                     specified response.

XYPLTCE       DBT    XY plot input data block, complex flutter.

XYPLOT        FMS    X-Y plot generator.

XYPLOT        IC     Request to generate X-Y plots.

XYPLTF        DBT    XYPLOT input data block. (D-8, D-11)

XYPLTFA       DBT    XYPLOT input data block. (D-8, D-11]

XYPLTR        DBT    XYPLOT input data block. (D-8, D-11]

XYPLTT        DBT    XYPLOT input data block. (D-9, D-12]

XYPLTTA       DBT    XYPLOT input data block. (D-9, D-12)

XYPRINT       IC     Request to tabulate XY pairs on the printer.

XYPRNPLT      FMX    Dummy output module.

XYPTTA        DBT    XY plot input data block, aeroresponse.

XYPUNCH       IC     Request to punch XY pairs.

XYTRAN        FMS    XY output translator.

XYZ           IC     Requests X, Y, and Z vectors for deformed structure plot.

XZ            IC     Requests X and Z vectors for deformed structure plot.
=PAGE=
Y             IC     Requests Y vector for deformed structure plot.

Y             M      Used in parameter section of DMAP statement. Indicates that
                     parameter may be given an initial value with a PARAM bulk
                     data card.

YAXIS         IC     Request for drawing of Y-axis.

YBDIVISIONS   IC     Request for division marking on Y-axis of lower half frame.

YBGRID LINES  IC     Request for grid lines to be drawn on Y-axis of lower half
                     frame.

YBINTERCEPT   IC     Specifies intercept of X-axis on Y-axis on lower half
                     frame.

YBLOG         IC     Request for logarithmic scales in Y-direction on lower half
                     frame.

YBMAX         IC     Do not plot points whose Y value lies above this value for
                     lower half frame.

YBMIN         IC     Do not plot points whose Y value lies below this value for
                     lower half frame.

YBS           DBM    Scalar multiple of YS matrix. Used in Differential
                     Stiffness Rigid Format only. (D-4).

YBTITLE       IC     Y-axis title on lower half frame.

YBVALUE PRINT IC     Request to suppress labeling tic marks over the specified
SKIP                 interval.

YDIVISIONS    IC     Request for division marking on Y-axis.

YES           IA     Option used on CHKPNT card, indicates that checkpoint is
                     desired.

YGRID LINES   IC     Request for grid lines to be drawn on Y-axis.

YINTERCEPT    IC     Specifies intercept of X-axis on Y-axis.

YLOG          IC     Request for logarithmic scales in Y-direction.

YMAX          IC     Do not plot points whose Y value lies above this value.

YMIN          IC     Do not plot points whose Y value lies below this value.

YPAPER        IC     Specifies length of paper in Y-direction for table plotter.

YS            DBM    {Y } - Constrained displacement vector.
                       s

YTDIVISIONS   IC     Request for division marking on Y-axis for upper half
                     frame.

YTGRID LINES  IC     Request for grid lines to be drawn on Y-axis for upper half
                     frame.

YTINTERCEPT   IC     Specifies intercept of X-axis on Y-axis for upper half
                     frame.

YTITLE        IC     Y-axis title.

YTLOG         IC     Request for logarithmic scales in Y-direction for upper
                     half frame.

YTMAX         IC     Do not plot points whose Y value lies above this value for
                     upper half frame.

YTMIN         IC     Do not plot points whose Y value lies below this value for
                     upper half frame.

YTITLE        IC     Y-axis title for upper half frame.

YTVALUE PRINT IC     Request to suppress labeling tic marks over the specified
SKIP                 interval for upper half frame.

YVALUE PRINT  IC     Request to suppress labeling tic marks over the specified
SKIP                 interval.

YZ            IC     Requests Y and Z vectors for deformed structure plot.

