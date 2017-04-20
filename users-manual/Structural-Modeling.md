
# 1.1 Introduction

NASTRAN embodies a lumped element approach, wherein the distributed
physical properties of a structure are represented by a model
consisting of a finite number of idealized substructures or elements
that are interconnected at a finite number of grid points, to which
loads are applied. All input and output data pertain to the idealized
structural model. The major components in the definition and loading
of a structural model are indicated in Figure 1.1-1.

As indicated in Figure 1.1-1, the grid point definition forms the
basic framework for the structural model. All other parts of the
structural model are referenced either directly or indirectly to the
grid points.

Two general types of grid points are used in defining the structural
model. They are:

1. Geometric grid point - a point in three-dimensional space at which
   three components of translation and three components of rotation
   are defined. The coordinates of each grid point are specified by
   you.
2. Scalar point - a point in vector space at which one degree of
   freedom is defined. Scalar points can be coupled to geometric grid
   points by means of scalar elements and by constraint relationships.

The structural element is a convenient means for specifying many of
the properties of the structure, including material properties, mass
distribution, and some types of applied loads. In static analysis by
the displacement method, stiffness properties are input exclusively by
means of structural elements. Mass properties (used in the generation
of gravity and inertia loads) are input either as properties of
structural elements or as properties of grid points. In dynamic
analysis, mass, damping, and stiffness properties may be input either
as the properties of structural elements or as the properties of grid
points (direct input matrices).

Structural elements are defined on connection cards by referencing
grid points, as indicated on Figure 1.1-1. In a few cases, all of the
information required to generate the structural matrices for the
element is given on the connection card. In most cases the connection
card refers to a property card, on which the cross-sectional
properties of the element are given. The property card in turn refers
to a material card which gives the material properties. If some of the
material properties are stress dependent or temperature dependent, a
further reference is made to tables for this information.

Various kinds of constraints can be applied to the grid
points. Single- point constraints are used to specify boundary
conditions, including enforced displacements of grid
points. Multipoint constraints and rigid elements are used to specify
linear relationships among selected degrees of freedom. Omitted
points are used as a tool in matrix partitioning and for reducing the
number of degrees of freedom used in dynamic analysis. Free-body
supports are used to remove stress-free motions in static analysis and
to evaluate the free-body inertia properties of the structural model.

Static loads may be applied to the structural model by concentrated
loads at grid points, pressure loads on surfaces, or indirectly, by
means of the mass and thermal expansion properties of structural
elements or enforced deformations of one-dimensional structural
elements. Due to the great variety of possible sources for dynamic
loading, only general forms of loads are provided for use in dynamic
analysis.

The following sections describe the general procedures for defining
structural models. Detailed instructions for each of the bulk data
cards and case control cards are given in Section 2. Additional
information on the case control cards and use of parameters is given
for each rigid format in Section 3.

![Structural model](figures/structural-model.png "Structural model")

**Figure 1.1-1. Structural model**

# 1.2 Grid Points

## 1.2.1 Grid Point Definition

Geometric grid points are defined on GRID bulk data cards by
specifying their coordinates in either the basic or a local coordinate
system. The implicitly defined basic coordinate system is rectangular,
except when using axisymmetric elements. Local coordinate systems may
be rectangular, cylindrical, or spherical. Each local system must be
related directly or indirectly to the basic coordinate system. The
CORD1C, CORD1R, and CORD1S cards are used to define cylindrical,
rectangular, and spherical local coordinate systems, respectively, in
terms of three geometric grid points which have been previously
defined. The CORD2C, CORD2R, and CORD2S cards are used to define
cylindrical, rectangular, and spherical local coordinate systems,
respectively, in terms of the coordinates of three points in a
previously defined coordinate system.

Six rectangular displacement components (3 translations and 3
rotations) are defined at each grid point. The local coordinate system
used to define the directions of motion may be different from the
local coordinate system used to locate the grid point. Both the
location coordinate system and the displacement coordinate system are
specified on the GRID card for each geometric grid point. The
orientation of displacement components depends on the type of local
coordinate system used to define the displacement components. If the
defining local system is rectangular, the displacement system is
parallel to the local system and is independent of the grid point
location as indicated in Figure 1.2-1a. If the local system is
cylindrical, the displacement components are in the radial,
tangential, and axial directions as indicated in Figure 1.2-1b. If the
local system is spherical, the displacement components are in the
radial, meridional, and azimuthal directions as indicated in Figure
1.2-1c. Each geometric grid point may have a unique displacement
coordinate system associated with it. The collection of all
displacement coordinate systems is known as the global coordinate
system. All matrices are formed and all displacements are output in
the global coordinate system. The symbols T1, T2, and T3 on the
printed output indicate translations in the 1, 2, and 3-directions,
respectively, for each grid point. The symbols R1, R2, and R3
indicate rotations (in radians) about the three axes.

Provision is also made on the GRID card to apply single-point
constraints to any of the displacement components. Any constraints
specified on the GRID card will be automatically used for all
solutions. Constraints specified on the GRID card are usually
restricted to those degrees of freedom that will not be elastically
constrained and hence must be removed from the model in order to avoid
singularities in the stiffness matrix.

The GRDSET card is provided to avoid the necessity of repeating the
specification of location coordinate systems, displacement coordinate
systems, and single-point constraints, when all, or many, of the GRID
cards have the same entries for these items. When any of the three
items are specified on the GRDSET card, the entries are used to
replace blank fields on the GRID card for these items. This feature is
useful in the case of such problems as space trusses where one wishes
to remove all of the rotational degrees of freedom or in the case of
plane structures where one wishes to remove all of the out-of-plane or
all of the in-plane motions.

Scalar points are defined either on an SPOINT card or by reference on
a connection card for a scalar element. SPOINT cards are used
primarily to define scalar points appearing in constraint equations,
but to which no structural elements are connected. A scalar point is
implicitly defined if it is used as a connection point for any scalar
element. Special scalar points, called "extra points", may be
introduced for dynamic analyses. Extra points are used in connection
with transfer functions and other forms of direct matrix input used in
dynamic analyses and are defined on EPOINT cards.

GRIDB is a variation of the GRID card that is used to define a point
on a fluid-structure interface (see Section 1.7).

*(a) Rectangular*
![Rectangular](figures/rectangular-coordinate-system.png "Rectangular")

*(b) Cylindrical*
![Cylindrical](figures/cylindrical-coordinate-system.png "Cylindrical")

*(c) Spherical*
![Spherical](figures/spherical-coordinate-system.png "Spherical")

**Figure 1.2-1. Displacement coordinate systems**

## 1.2.2 Grid Point Sequencing

The external identification numbers used for grid points may be
selected in any manner you desire. However, in order to reduce the
number of active columns, and, hence, to substantially reduce
computing times when using the displacement method, the internal
sequencing of the grid points must not be arbitrary. The best
decomposition and equation solution times are obtained if the grid
points are sequenced in such a manner as to create matrices having
small numbers of active columns (see Section 2.2 of the Theoretical
Manual for a discussion of active columns and the decomposition
algorithm). The decomposition time is proportional to the sum of the
squares of the number of active columns in each row of the triangular
factor. The equation solution time (forward/backward substitution) is
proportional to the number of nonzero terms in the triangular factor.

## 1.2.2.1 Manual Grid Point Resequencing

In order to allow arbitrary grid point numbers and still preserve
sparsity in the triangular decomposition factor to the greatest extent
possible, provision is made for you to resequence the grid point
numbers for internal operations. This feature also makes it possible
to easily change the sequence if a poor initial choice is made. All
output associated with grid points is identified with the external
grid point numbers. The SEQGP card is used to resequence geometric
grid points and scalar points. The SEQEP card is used to sequence the
extra points in with the previously sequenced grid points and scalar
points.

In selecting the grid point sequencing, it is not important to find
the best sequence; rather it is usually quite satisfactory to find a
good sequence, and to avoid bad sequences that create unreasonably
large numbers of active columns. For many problems a sequence which
will result in a band matrix is a reasonably good choice, but not
necessarily the best. Also, sequences which result in small numbers of
columns with nonzero terms are usually good but not necessarily the
best. A sequence with a larger number of nonzero columns will
frequently have a smaller number of nonzero operations in the
decomposition when significant passive regions exist within the active
columns (see Section 2.2 of the Theoretical Manual).

Examples of proper grid point sequencing for one-dimensional systems
are shown in Figure 1.2-2. For open loops, a consecutive numbering
system should be used as shown in Figure 1.2-2a. This sequencing will
result in a narrow band matrix with no new nonzero terms created
during the triangular decomposition. Generally, there is an
improvement in the accumulated round off error if the grid points are
sequenced from the flexible end to the stiff end.

For closed loops, the grid points may be sequenced either as shown in
Figure 1.2-2b or as shown in Figure 1.2-2c. If the sequencing is as
shown in Figure 1.2-2b, the semiband will be twice that of the model
shown in Figure 1.2-2a. The matrix will initially contain a number of
zeroes within the band which will become nonzero as the decomposition
proceeds. If the sequencing is as shown in Figure 1.2-2c, the band
portion of the matrix will be the same as that for Figure
1.2-2a. However, the connection between grid points 1 and 8 will
create a number of active columns on the right hand side of the
matrix. The solution times will be the same for the sequence shown in
Figure 1.2-2b or 1.2-2c, because the number of active columns in each
sequence is the same.

Examples of grid point sequencing for surfaces are shown in Figure
1.2-3. For plain or curved surfaces with a pattern of grid points
that tends to be rectangular, the sequencing shown in Figure 1.2-3a
will result in a band matrix having good solution times. The semiband
will be proportional to the number of grid points along the short
direction of the pattern. If the pattern of grid points shown in
Figure 1.2-3a is made into a closed surface by connecting grid points
1 and 17, 2 and 18, etc., a number of active columns equal to the
semiband will be created. If the number of grid points in the
circumferential direction is greater than twice the number in the
axial direction, the sequencing indicated in Figure 1.2-3a is a good
one. However, if the number of grid points in the circumferential
direction is less than twice the number in the axial direction, the
use of consecutive numbering in the circumferential direction is more
efficient. An alternate sequencing for a closed loop is shown in
Figure 1.2-3b, where the semiband is proportional to twice the number
of grid points in a row. For cylindrical or similar closed surfaces,
the sequencing shown in Figure 1.2-3b has no advantage over that shown
in Figure 1.2-3a, as the total number of active columns will be the
same in either case.

With the exception of the central point, sequencing considerations for
the radial pattern shown in Figure 1.2-3c are similar to those for the
rectangular patterns shown in Figures 3a and 3b. The central point
must be sequenced last in order to limit the number of active columns
associated with this point to the number of degrees of freedom at the
central point. If the central point is sequenced first, the number of
active columns associated with the central point will be proportional
to the number of radial lines. If there are more grid points on a
radial line than on a circumferential line, the consecutive numbering
should extend in the circumferential direction beginning with the
outermost circumferential ring. In this case, the semiband is
proportional to the number of grid points on a circumferential line
and there will be no active columns on the right hand side of the
matrix. If the grid points form a full circular pattern, the closure
will create a number of active columns proportional to the number of
grid points on a radial line if the grid points are numbered as shown
in Figure 1.2-3c. Proper sequencing for a full circular pattern is
similar to that discussed for the rectangular arrays shown in Figures
3a and 3b for closed surfaces.

Sequencing problems for actual structural models can frequently be
handled by considering the model as consisting of several
substructures. Each substructure is first numbered in the most
efficient manner. The substructures are then connected so as to create
the minimum number of active columns. The grid points at the interface
between two substructures are usually given numbers near the end of
the sequence for the first substructure and as near the beginning of
the sequence for the second substructure as is convenient.

Figure 1.2-4 shows a good sequence for the substructure approach. Grid
points 1 through 9 are associated with the first substructure, and
grid points 10 through 30 are associated with the second
substructure. In the example, each of the substructures was sequenced
for band matrices. However, other schemes could also be considered for
sequencing the individual substructures. Figure 1.2-5 shows the
nonzero terms in the triangular factor. The X's indicate terms which
are nonzero in the original matrix. The zeros indicate nonzero terms
created during the decomposition. The maximum number of active columns
for any pivotal row is only five, and this occurs in only three rows
near the middle of the matrix for the second substructure. All other
pivotal rows have four or less active columns.

Figure 1.2-6 indicates the grid point sequencing using substructuring
techniques for a square model, and Figure 1.2-7 shows the nonzero
terms in the triangular factor. If the square model were sequenced for
a band matrix, the number of nonzero terms in the triangular factor
would be 129, whereas Figure 1.2-7 contains only 102 nonzero
terms. The time for the forward/backward substitution operation is
directly proportional to the number of nonzero terms in the triangular
factor. Consequently, the time for the forward/backward substitution
operation when the square array is ordered as shown in Figure 1.2-7 is
only about 80% of that when the array is ordered for a band matrix.
The number of multiplications for a decomposition when ordered for a
band is 294, whereas the number indicated in Figure 1.2-7 is
only 177. This indicates that the time for the decomposition when
ordered as shown in Figure 1.2-6 is only 60% of that when ordered for
a band.

Although scalar points are defined only in vector space, the pattern
of the connections is used in a manner similar to that of geometric
grid points for sequencing scalar points among themselves or with
geometric grid points. Since scalar points introduced for dynamic
analysis (extra points) are defined in connection with direct input
matrices, the sequencing of these points is determined by direct
reference to the positions of the added terms in the dynamic matrices.

*(a) Consecutive numbering system for open loops.*
![Open Loop](figures/open-loop.png "Open Loop")

*(b) Sequencing of grid points for a closed loop (method 1).*
![Closed Loop Method 1](figures/closed-loop-method-1.png "Closed Loop Method 1")

*(c) Sequencing of grid points for a closed loop (method 2).*
![Closed Loop Method 2](figures/closed-loop-method-2.png "Closed Loop Method 2")

**Figure 1.2-2. Grid point sequencing for one-dimensional systems**

*(a) Grid-point sequencing for a rectangular surface (method 1).*
![Rectangular Surface Method 1](figures/rectangular-surface-method-1.png
    "Rectangular Surface Method 1")

*(b) Grid-point sequencing for a rectangular surface (method 2).*
![Rectangular Surface Method 2](figures/rectangular-surface-method-2.png
    "Rectangular Surface Method 2")

*(c) Grid-point sequencing for a radial pattern.*
![Radial Pattern](figures/radial-pattern.png "Radial Pattern")

**Figure 1.2-3. Grid point sequencing for surfaces**

![Substructure Sequence](figures/substructure-sequence.png "Substructure Sequence")

**Figure 1.2-4. Grid point sequencing for substructures**

>     X X   X
>       X X 0 X
>         X 0 0 X
>           X X 0 X
>             X X 0 X
>               X 0 0 X
>                 X X 0             X
>                   X X             0     X
>                     X             0     0     X
>                       X X   X
>                         X X 0 X
>                           X 0 0 X
>                             X X 0 X
>                               X X 0 X
>                                 X 0 0 X
>                                   X X 0 X     0
>                                     X X 0 X   0
>                                       X 0 0 X 0
>                                         X X 0 X
>                                           X X 0 X
>          (Symmetric)                        X 0 0 X
>                                               X X 0 X
>                                                 X X 0 X
>                                                   X 0 0 X
>                                                     X X 0 X
>                                                       X X 0 X
>                                                         X 0 0 X
>                                                           X X 0
>                                                             X X
>                                                               X

**Figure 1.2-5. Matrix for substructure example**

![Square Model](figures/square-model.png "Square Model")

**Figure 1.2-6. Grid point sequencing for square model**

>          X X X
>            X 0 X                             X
>              X X                         X   0
>                X                         0 X 0 X
>                  X X X
>                    X 0 X                     X
>                      X X                     0   X
>                        X                     0 X 0 X
>                          X X X
>                            X 0 X                     X
>                              X X                 X   0
>                                X                 0 X 0 X
>                                  X X X
>                                    X 0 X             X
>                                      X X X           0
>                                        X 0 X         0 X
>               (Symmetric)                X X 0 0
>                                            X 0 0     0 0 X
>                                              X X 0 0 0 0 0
>                                                X 0 0 0 0 X
>                                                  X X 0 0 0
>                                                    X 0 0 X
>                                                      X X 0
>                                                        X X
>                                                          X

**Figure 1.2-7. Matrix for square model example**

## 1.2.2.2 Automatic Grid Point Resequencing Using the BANDIT Procedure

If you want reduced matrix reduction and equation solution times, you
can manually resequence your grid points by the use of SEQGP cards as
per the guidelines outlined in the previous section. However, in order
to relieve you of the burden of having to do so, an automatic
resequencing capability has been provided in NASTRAN. This capability
involves the use of the BANDIT procedure in NASTRAN. (See Reference 1
for details of the BANDIT procedure and Reference 2 for details of the
manner in which it has been implemented in NASTRAN.)

The BANDIT procedure in automatically invoked in NASTRAN for all runs
(except those indicated in Sections 1.2.2.2.2 and 1.2.2.2.3), unless
specifically suppressed by you. (See the description of the BANDIT
options in the next section.) The result of the BANDIT operations is a
set of SEQGP cards that are automatically generated by the
program. These SEQGP cards are added to your input data (replacing any
SEQGP cards already input, if so specified) for subsequent processing
by the program.

### 1.2.2.2.1 BANDIT Options

The execution of the BANDIT operations in NASTRAN is controlled by
several parameters. These parameters can be specified by means of the
NASTRAN card and are fully described in Section 2.1. All of these
parameters have default values selected so that you normally do not
have to explicitly specify any of them.

NASTRAN provides two methods to skip over the BANDIT
operations. First, the NASTRAN BANDIT = -1 option can be used. The
second method is to include one or more SEQGP cards in the Bulk Data
Deck. In this second method, BANDIT would terminate since you have
already stated your choice of SEQGP resequencing cards. However, the
NASTRAN BANDTRUN = 1 option can be used to force BANDIT to generate
new SEQGP cards to replace the old SEQGP set already in the input Bulk
Data Deck. In all instances when BANDIT is executed, NASTRAN will
issue a page of summary to keep you informed of the basic resequencing
computations. You may refer to Reference 1 for the definition of the
technical terms used.

The BANDIT procedure automatically counts the number of grid points
used in a NASTRAN job and sets up the exact array dimensions needed
for its internal computations. However, if your structural model uses
more grid points in the connecting elements than the total number of
grid points as defined on the GRID cards, BANDIT will issue a fatal
message and terminate the job. In the case where non-active grid
points (that is, grid points defined on the GRID cards but nowhere
used in the model) do exist, BANDIT will add them to the end of the
SEQGP cards, and their presence will not cause termination of a job.
(If necessary, the NASTRAN HICORE parameter can be used on the UNIVAC
version to increase the amount of open core available for the BANDIT
operations.)

Multipoint constraints (MPCs) and rigid elements are included in the
BANDIT computations only when the BANDTMPC = 1 (or 2) option is
selected. (The use of the dependent grid points of MPCs and/or rigid
elements is controlled by the BANDTDEP option.) However, as noted in
Reference 1, it should be emphasized here that only in rare cases
would it make sense to let BANDIT process MPCs and rigid elements. The
main reasons for this are that BANDIT does not consider individual
degrees of freedom and, in addition, cannot distinguish one MPC set
from another.

### 1.2.2.2.2 Cases for Which BANDIT Computations are Skipped

The BANDIT computations in NASTRAN are unconditionally skipped over if
any of the following conditions exists:

1. There are errors in input data.
2. The Bulk Data Deck contains any of the following types of input:
    - Axisymmetric (CONEAX, TRAPAX, or TRIAAX) elements
    - Fluid (FLUID2, FLUID3, or FLUID4) elements
    - DMI (Direct Matrix Input) data
3. It is a substructure Phase 2 run.

### 1.2.2.2.3 BANDIT in Restarts

At the beginning of a NASTRAN job, the Preface (or Link 1) modules
read and process the Executive, Case Control, and Bulk Data decks. The
SEQGP cards generated by BANDIT are added directly to the NASTRAN data
base (specifically, the GEOM1 file) at a later stage. Since these
SEQGP cards are not part of the original Bulk Data Deck, they are not
directly written on to the NPTP (New Problem Tape) in a checkpoint run
and, therefore, are not available as such for use on the OPTP (Old
Problem Tape) in a restart.

In the light of the above comments, the following points about the use
of BANDIT in NASTRAN restarts should be noted:

1. BANDIT is automatically skipped if the restart job has no input
   data changes with respect to the checkpoint job. However, the
   previously generated SEQGP cards, if any, are already absorbed into
   the NASTRAN data base (data blocks such as EQEXIN, SIL, etc.). A
   message is printed to inform you that the BANDIT computations are
   not performed. (BANDIT can be executed if the restart job contains
   one or more of the appropriate BANDIT options on the NASTRAN card,
   for example, NASTRAN BANDMTH = 2.)
2. BANDIT is executed (except for the cases indicated in Section
   1.2.2.2.2) if the restart job has input data changes with respect
   to the checkpoint job, unless specifically suppressed by you. (The
   BANDIT = -1 option on the NASTRAN card can be used to stop BANDIT
   execution unconditionally.)

## 1.2.3 Grid Point Properties

Some of the characteristics of the structural model are introduced as
properties of grid points, rather than as properties of structural
elements. Any of the various forms of direct matrix input are
considered as describing the structural model in terms of properties
of grid points.

Thermal fields are defined by specifying the temperatures at grid
points. The TEMP card is used to specify the temperature at grid
points for use in connection with thermal loading and
temperature-dependent material properties. The TEMPD card is used to
specify a default temperature, in order to avoid a large number of
duplicate entries on a TEMP card when the temperature is uniform over
a large portion of the structure. The TEMPAX card is used for conical
shell problems.

Mass properties may be input as properties of grid points by using the
concentrated mass element (see Section 5.5 of the Theoretical
Manual). The CONM1 card is used to define a 6x6 matrix of mass
coefficients at a geometric grid point in any selected coordinate
system. The CONM2 card is used to define a concentrated mass at a
geometric grid point in terms of its mass, the three coordinates of
its center of gravity, the three moments of inertia about its center
of gravity, and its three products of inertia, referred to any
selected coordinate system.

In dynamic analysis, mass, damping and stiffness properties may be
provided, in part or entirely, as properties of grid points through
the use of direct input matrices. The DMIG card is used to define
direct input matrices for use in dynamic analysis. These matrices may
be associated with components of geometric grid points, scalar points,
or extra points introduced for dynamic analysis. The TF card is used
to define transfer functions that are internally converted to direct
matrix input. The DMIAX card is an alternate form of direct matrix
input that is used for hydroelastic problems (see Section 1.7).

## References

1. Everstine, G. C., "BANDIT User's Guide", COSMIC Program
   No. DOD-00033, May 1978.
2. Chan, G. C., "BANDIT in NASTRAN," Eleventh NASTRAN Users'
   Colloquium, NASA Conference Publication, May 1983, San Francisco,
   California, pp. 1-5.

# 1.3 STRUCTURAL ELEMENTS

## 1.3.1 Element Definition

Structural elements are defined on connection cards that identify the
grid points to which the elements are connected. The mnemonics for all
such cards have a prefix of the letter "C", followed by an indication
of the type of element, such as CBAR and CROD. The order of the grid
point identification defines the positive direction of the axis of a
one-dimensional element and the positive surface of a plate
element. The connection cards include additional orientation
information when required. Except for the simplest elements, each
connection card references a property definition card. If many
elements have the same properties, this system of referencing
eliminates a large number of duplicate entries.

The property definition cards define geometric properties such as
thicknesses, cross-sectional areas, and moments of inertia. The
mnemonics for all such cards have a prefix of the letter "P", followed
by some, or all, of the characters used on the associated connection
card, such as PBAR and PROD.  Other included items are the
nonstructural mass and the location of points where stresses will be
calculated. Except for the simplest elements, each property definition
card will reference a material property card.

In some cases, the same finite element can be defined by using
different bulk data cards. These alternate cards have been provided
for your convenience. In the case of a rod element, the normal
definition is accomplished with a connection card (CROD) which
references a property card (PROD). However, an alternate definition
uses a CONROD card which combines connection and property information
on a single card. This is more convenient if a large number of rod
elements all have different properties.

In the case of plate elements, a different property card is provided
for each type of element, such as membrane or sandwich plates. Thus,
each property card contains only the information required for a single
type of plate element, and in most cases, a single card has sufficient
space for all of the property information. In order to maintain
uniformity in the relationship between connection cards and property
cards, a number of connection card types contain the same information,
such as the connection cards for the various types of triangular
elements. Also, the property cards for triangular and quadrilateral
elements of the same type contain the same information.

The material property definition cards are used to define the
properties for each of the materials used in the structural model. The
MAT1 card is used to define the properties for isotropic
materials. The MAT1 card may be referenced by any of the structural
elements. The MATS1 card specifies table references for isotropic
material properties that are stress dependent. The TABLES1 card
defines a tabular stress-strain function for use in piecewise linear
analysis. The MATT1 card specifies table references for isotropic
material properties that are temperature dependent. The TABLEM1,
TABLEM2, TABLEM3, and TABLEM4 cards define four different types of
tabular functions for use in generating temperature-dependent material
properties.

The MAT2 card is used to define the properties for anisotropic
materials.  The MAT2 card may only be referenced by triangular or
quadrilateral membrane and bending elements. The MAT2 card specifies
the relationship between the inplane stresses and strains. The
material is assumed to be infinitely rigid in transverse shear. The
angle between the material coordinate system and the element
coordinate system is specified on the connection cards. The MATT2 card
specifies table references for anisotropic material properties that
are temperature dependent. This card may reference any of the TABLEM1,
TABLEM2, TABLEM3, or TABLEM4 cards.

The MAT3 card is used to define the properties for orthotropic
materials used in the modeling of axisymmetric shells. This card may
only be referenced by CTRIARG, CTRIAAX, CTRAPRG, CTRAPAX, and PTORDRG
cards. The MATT3 card specifies table references for use in generating
temperature-dependent properties for this type of material.

The GENEL card is used to define general elements whose properties are
defined in terms of deflection influence coefficients or stiffness
matrices, and which can be connected between any number of grid
points. One of the important uses of the general element is the
representation of part of a structure by means of experimentally
measured data. No output data is prepared for the general
element. Detail information on the general element is given in Section
5.7 of the Theoretical Manual.

Dummy elements are provided in order to allow you to investigate new
structural elements with a minimum expenditure of time and money. A
dummy element is defined with a CDUMi (i = index of element type, 1 <=
i <= 9) card and its properties are defined with the PDUMi card. The
ADUMi card is used to define the items on the connection and property
cards. Detailed instructions for coding dummy element routines are
given in Section 6.8.5 of the Programmer's Manual.

## 1.3.2 Beam Elements

### 1.3.2.1 Simple Beam or Bar Element

The simple beam or bar element is defined with a CBAR card and its
properties (constant over the length) are defined with a PBAR
card. The bar element includes extension, torsion, bending in two
perpendicular planes, and the associated shears. The shear center is
assumed to coincide with the elastic axis. Any five of the six forces
at either end of the element may be set equal to zero by using the pin
flags on the CBAR card. The integers 1 to 6 represent the axial force,
shearing force in Plane 1, shearing force in Plane 2, axial torque,
moment in Plane 2, and moment in Plane 1, respectively. The structural
and nonstructural mass of the bar are lumped at the ends of the
element, unless coupled mass is requested with a PARAM COUPMASS card
(see PARAM bulk data card). Theoretical aspects of the bar element are
treated in Section 5.2 of the Theoretical Manual.

The element coordinate system is shown in Figure 1.3-1a. End a is
offset from grid point a an amount measured by vector wa and end b is
offset from grid point b an amount measured by vector wb. The vectors
wa and wb are measured in the global coordinates of the connected grid
point. The x-axis of the element coordinate system is defined by a
line connecting end a to end b of the bar element. The orientation of
the bar element is described in terms of two reference planes. The
reference planes are defined with the aid of vector v. This vector may
be defined directly with three components in the global system at end
a of the bar or by a line drawn from end a to a third referenced grid
point. The first reference plane (Plane 1) is defined by the x-axis
and the vector v. The second reference plane (Plane 2) is defined by
the vector cross product (x x v) and the x-axis. The subscripts 1 and
2 refer to forces and geometric properties associated with bending in
planes 1 and 2, respectively. The reference planes are not necessarily
principal planes. The coincidence of the reference planes and the
principal planes is indicated by a zero product of inertia (I12) on
the PBAR card. If shearing deformations are included, the reference
axes and the principal axes must coincide. When pin flags and offsets
are used, the effect of the pin is to free the force at the end of the
element x-axis of the beam, not at the grid point. The positive
directions for element forces are shown in Figure 1.3-1b. The
following element forces, either real or complex (depending on the
rigid format), are output on request:

- Bending moments at both ends in the two reference planes.
- Shears in the two reference planes.
- Average axial force.
- Torque about the bar axis.

The following real element stresses are output on request:

- Average axial stress.
- Extensional stress due to bending at four points on the
  cross-section at both ends. (Optional; calculated only if you enter
  stress recovery points on PBAR card.)
- Maximum and minimum extensional stresses at both ends.
- Margins of safety in tension and compression for the whole
  element. (Optional; calculated only if you enter stress limits on
  MAT1 card.)

Tensile stresses are given a positive sign and compressive stresses a
negative sign. Only the average axial stress and the extensional
stresses due to bending are available as complex stresses. The stress
recovery coefficients on the PBAR card are used to locate points on
the cross-section for stress recovery. The subscript 1 is associated
with the distance of a stress recovery point from plane 2. The
subscript 2 is associated with the distance from plane 1.

The use of the BAROR card avoids unnecessary repetition of input when
a large number of bar elements either have the same property
identification number or have their reference axes oriented in the
same manner. This card is used to define default values on the CBAR
card for the property identification number and the orientation vector
for the reference axes. The default values are used only when the
corresponding fields on the CBAR card are blank.

### 1.3.2.2  Curved Beam or Elbow Element

The curved beam or elbow element is a three-dimensional element with
extension, torsion, and bending capabilities and the associated
shears. No offset of the elastic axis is allowed nor are pin releases
permitted to eliminate the connection between motions at the ends of
the element and the adjacent grid points.

The elbow element was initially developed to facilitate the analysis
of pipe networks by using it as a curved pipe element. However, the
input format is general enough to allow application to beams of
general cross section. An important assumption in the development of
the element is that the radius of curvature is much larger than the
cross section depth.

The element is defined with a CELBOW card and its properties (constant
over the length) are defined with a PELBOW card. There are six degrees
of freedom at each end of the element: translations in the local x, y,
z directions and rotations about the local x, y, z axes. The
structural and nonstructural mass of the element are lumped at the
ends of the element.

The specified properties of the elbow element are its area; its
moments of inertia, I1 and I2 (the product of inertia is assumed to be
zero); its torsional constant, J; the radius of curvature; the angle
between end-a and end-b; the factors K1 and K2 for computing
transverse shear stiffness; the nonstructural mass per unit length,
NSM; the stress intensification factor, C; and the flexibility
correction factors, Kx, Ky, and Kz. The stress intensification factor
C is applied to the bending stress only. The flexibility correction
factors Kx, Ky, and Kz are generally greater than 1.0 and are used as
divisors to reduce the respective moments of inertia. These are
discussed further towards the end of this section.

The material properties, obtained by reference to a materials
properties table, include the elastic moduli, E and G, density, rho,
and the thermal expansion coefficient, \\( \alpha \\), determined at the
average temperature of the element.

The plane of the element is defined by two grid points, A and B, and a
vector v from grid point A directed toward the center of
curvature. Plane 1 of the element cross section lies in this
plane. Plane 2 is normal to Plane 1 and contains the vector v. The
area moments of inertia, I1 and I2, are defined as for the BAR
element. The cross product of inertia, I12, is neglected. This
assumption requires that at least one axis of the element cross
section be an axis of symmetry.

The following element forces are output on request:

- Bending moments at both ends in the two reference planes
- Transverse shear force at both ends in the two reference planes
- Axial force at both ends
- Torque at both ends

The following element stresses are output on request:

- Average axial stress at both ends
- Bending stresses at four points on the cross section at both
  ends. The points are specified by you.
- Maximum and minimum extensional stresses at both ends.
- Margins of safety in tension and compression (Optional, output only
  if you enter stress limits on MAT1 card)

Stress Intensification Factor and Flexibility Correction Factors

When a plane pipe network, consisting of both straight and curved
sections, is analyzed by simple beam theory as an indeterminate
system, the computed support reactions are greater than actually would
be measured in an experiment. The apparent decrease in stiffness in
such a case is due to an ovalization of the pipe in the curved
sections. The ovalization also yields a stress distribution different
from that computed by simple beam theory.

When a curved beam or elbow element is used as a curved pipe element,
there are two factors available that can be specified to account for
the differences in its behavior compared to curved beams. These are
the stress intensification factor and the flexibility correction
factors.

The maximum stress, \\( \sigma_{max} \\), in a curved pipe element is
given by

\begin{equation} \sigma_{max} = C \frac{Mc}{I}\end{equation}

where C is a stress intensification factor,

    M = bending moment,
    c = fiber distance, and
    I = plane (area) moment of inertia of the cross section.

In general, the factor C mentioned above may be regarded as a stress
correction factor in curved beam analysis.

The effect of the ovalization of the pipe in curved sections is to
reduce the stiffness parameter EI (E: modulus of elasticity) of the
curved pipe to a fictitious value. Thus, for the elbow element,

\begin{equation}
(EI1)^{'} = \frac{EI1}{K\_y} \mbox{ , } (1.0 < K\_y) \mbox{, and}
\end{equation}

\begin{equation}
(EI2)^{'} = \frac{EI2}{K\_z} \mbox{ , } (1.0 < K\_z)
\end{equation}

where Ky and Kz are the stiffness correction factors corresponding to
planes 1 and 2, respectively. The stiffness correction factor, Kz,
corresponds to the torsional behavior and is generally taken to be
1.0.

## 1.3.3  Rod Element

The rod element is defined with a CROD card and its properties with a
PROD card. The rod element includes extensional and torsional
properties. The CONROD card is an alternate form that includes both
the connection and property information on a single card. The tube
element is a specialized form that is assumed to have a circular
cross-section. The tube element is defined with a CTUBE card and its
properties with a PTUBE card. The structural and nonstructural mass of
the rod are lumped at the adjacent grid points unless coupled mass is
requested with the PARAM COUPMASS card (see PARAM bulk data
card). Theoretical aspects of the rod element are treated in Section
5.2 of the Theoretical Manual.

The x-axis of the element coordinate system is defined by a line
connecting end a to end b as shown in Figure 1.3-2. The axial force
and torque are output on request in either the real or complex
form. The positive directions for these forces are indicated in Figure
1.3-2. The following real element stresses are output on request:

- Axial stress
- Torsional stress
- Margin of safety for axial stress
- Margin of safety for torsional stress.

Positive directions are the same as those indicated in Figure 1.3-2
for element forces. Only the axial stress and the torsional stress are
available as complex stresses.

Another kind of rod element is the viscous damper, which has
extensional and torsional viscous damping properties rather than
stiffness properties. The viscous damper element is defined with a
CVISC card and its properties with a PVISC card. This element is used
in the direct formulation of dynamic matrices.

## 1.3.4 Shear Panels and Twist Panels

The shear panel is defined with a CSHEAR card and its properties with
a PSHEAR card. A shear panel is a two-dimensional structural element
that resists the action of tangential forces applied to its edges, but
does not resist the action of normal forces. The structural and
nonstructural mass of the shear panel are lumped at the connected grid
points. Details of the shear panel element are discussed in Section
5.3 of the Theoretical Manual.

The element coordinate system for a shear panel is shown in Figure
1.3-3a.  The integers 1, 2, 3, and 4 refer to the order of the
connected grid points on the CSHEAR card. The element forces are
output on request in either the real or complex form. The positive
directions for these forces are indicated in Figure 1.3-3b. These
forces consist of the forces applied to the element at the corners in
the direction of the sides, kick forces at the corners in a direction
normal to the plane formed by the two adjacent edges, and "shear
flows" (force per unit length) along the four edges. The shear
stresses are calculated at the corners in skewed coordinates parallel
to the exterior edges. The average of the four corner stresses and the
maximum stress are output on request in either the real or complex
form. A margin of safety is also output when the stresses are real.

The twist panel performs the same function for bending action that the
shear panel performs for membrane action. The twist panel is defined
with a CTWIST card and its properties with a PTWIST card. In
calculating the stiffness matrix, a twist panel is assumed to be
solid. For built-up panels, the thickness in the PTWIST card must be
adjusted to give the correct moment of inertia of the
cross-section. If mass calculations are being made, the density will
also have to be adjusted on a MAT1 card. The element coordinate system
and directions for positive forces are shown in Figure 1.3-4. Stress
recovery is similar to that for shear panels.

## 1.3.5  Plate and Membrane Elements

NASTRAN includes two different shapes of plate and membrane elements
(triangular and quadrilateral) and two different stress systems
(inplane and bending) which are uncoupled. There are different forms
of elements available that are defined by connection cards as follows:

1. Plate (Bending) Elements

   a. CTRBSC - basic unit from which the bending properties of the
   other plate elements are formed.

   b. CTRPLT - triangular element with zero inplane stiffness and
   finite bending stiffness.

   c. CTRPLT1 - a higher order triangular element with zero inplane
   stiffness and finite bending stiffness. Uses quintic polynomial
   representation for transverse displacements and bilinear variation
   for temperature and thickness.

   d. CQDPLT - quadrilateral element with zero inplane stiffness and
   finite bending stiffness.

2. Membrane (Inplane) Elements

   a. CTRMEM - triangular element with finite inplane stiffness and
   zero bending stiffness.

   b. CTRIM6 - triangular element with finite inplane stiffness and
   zero bending stiffness. Uses quadratic polynomial representation
   for membrane displacements and bilinear variation for temperature
   and thickness.

   c. CQDMEM - quadrilateral element consisting of four overlapping
   CTRMEM elements.

   d. CQDMEM1 - an isoparametric quadrilateral membrane element.

   e. CQDMEM2 - a quadrilateral membrane element consisting of four
   non- overlapping CTRMEM elements.

   f. CIS2D8 - a quadriparabolic isoparametric membrane element. May
   be reduced to a triangular element under specified conditions.

3. Plate and Membrane Elements

   a. CTRIA1 - triangular element with both inplane and bending
   stiffness. It is designed for sandwich plates which can have
   different materials referenced for membrane, bending, and
   transverse shear properties.

   b. CTRIA2 - triangular element with both inplane and bending
   stiffness that assumes a solid homogeneous cross-section.

   c. CQUAD1 - quadrilateral element with both inplane and bending
   stiffness. It is designed for sandwich plates which can have
   different materials referenced for membrane, bending, and
   transverse shear properties.

   d. CQUAD2 - quadrilateral element with both inplane and bending
   stiffness that assumes a solid homogeneous cross-section.

Theoretical aspects of these elements are treated in Section 5.8 of
the Theoretical Manual.

The properties for the above elements are defined on their associated
Pxxxxxx cards (PTRBSC, PTRPLT, etc.). All of the properties of the
elements are assumed uniform over their surfaces, except for the
CTRIM6 and CTRPLT1 elements. Anisotropic material may be specified for
all these elements.  Transverse shear flexibility may be included for
all bending elements on an optional basis, except for homogeneous
elements (CTRIA2 and CQUAD2), where this effect is automatically
included. Structural mass is calculated only for elements that specify
a membrane thickness and is based only on the membrane
thickness. Nonstructural mass can be specified for all plate elements,
except the basic bending triangle. Only lumped mass procedures are
used for membrane elements, except for the CIS2D8 element. Coupled
mass procedures may be requested for elements that include bending
stiffness with the PARAM COUPMASS card (see PARAM bulk data
card). Differential stiffness matrices are generated for the following
elements: CTRMEM, CTRIA1, CTRIA2, CQDMEM, CQUAD1, CQUAD2.  The
following elements may have nonlinear material characteristics in
Piecewise Linear Analysis: CTRMEM, CTRIA1, CTRIA2, CQDMEM, CQUAD1,
CQUAD2.

The element coordinate systems for the triangular and quadrilateral
elements are shown in Figure 1.3-5. The integers 1, 2, 3, and 4 refer
to the order of the connected grid points on the connection cards
defining the elements. A similar connection scheme for elements with
mid-side grid points would be defined by six or eight integers on the
connection card. The angle é is the orientation angle for anisotropic
materials.

Average values of element forces are calculated for all plate elements
(except the CTRPLT1) having a finite bending stiffness. The element
forces for the CTRPLT1 are calculated at the corners and centroid of
the element. The positive directions for plate element forces in the
element coordinate system are shown in Figure 1.3-6a. The following
element forces per unit of length, either real or complex, are output
on request:

-  Bending moments on the x and y faces.
-  Twisting moment.
-  Shear forces on the x and y faces.

The CQDMEM2 is the only membrane element for which element forces are
calculated. The positive directions for these forces are shown in
Figure 1.3- 3b, and the force output has the same interpretation as
the force output for the shear panel discussed previously.

Average values of the membrane stresses are calculated for the
triangular and quadrilateral membrane elements, with the exception of
the CQDMEM1 and CTRIM6 elements. For the CQDMEM1 element, in which the
stress field varies, the stresses are evaluated at the intersection of
diagonals (in a mean plane if the element is warped.) For the CTRIM6
element, the stresses are calculated at the corners and centroid of
the element. The positive directions for the membrane stresses are
shown in Figure 1.3-6b. The stresses for the CQDMEM2 element are
calculated in the material coordinate system. The material coordinate
system is defined by the material orientation angle on the CQDMEM2
card. The stresses for all other membrane elements are calculated in
the element coordinate system. For the CIS2D8 element, the stresses
are computed at the Gaussian quadrature points and extrapolated to the
grid points.

The following real membrane stresses are output on request:

-  Normal stresses in the x and y directions
-  Shear stress on the x face in the y direction
-  Angle between the x-axis and the major principal axis
-  Major and minor principal stresses
-  Maximum shear stress

Only the normal stresses and shearing stress are available in the
complex form.

If an element has bending stiffness, the average stresses are
calculated on the two faces of the plate for homogeneous plates and at
two specified points on the cross-section for other plate
elements. The distances to the specified points are given on the
property cards. The positive directions for these fiber distances are
defined according to the right-hand sequence of the grid points
specified on the connection card. These distances are identified in
the output and must be nonzero in order to obtain nonzero stress
output. The same stresses are calculated for each of the faces as are
calculated for membrane elements.

In the case of composite plate elements (CTRIA1, CTRIA21, CQUAD1, and
CQUAD2 only), the stresses mentioned above can also be requested in a
material coordinate system which is specified on a MAT1 or MAT2
card. In place of the fiber distances, the output in this case
identifies the specified material coordinate system as well as an
output code. This latter code is set to 1 or 2 according as the
material x-axis or the y-axis is chosen as the reference axis.

The element stresses in material coordinate system computed above (for
CTRIA1, CTRIA2, CQUAD1, and CQUAD2 elements) can also be requested at
the connected grid points. These stresses (at grid points) are
obtained by interpolation. The output code in this case is set to
(10*N + projection code) where N is the number of independent points
used in the interpolation and the projection code is an integer which
is set to 1, 2, or 3 according as the material x-axis, y-axis, or the
z-axis is normal to projection.

In the case of composite plate elements (CTRIA1, CTRIA2, CQUAD1, and
CQUAD2 only), strains and curvatures are also output on request. The
options available and the output formats are similar to those
available in the case of stresses as described above.

The quadrilateral elements are intended for use when the surfaces are
reasonably flat and the geometry is nearly rectangular. For these
conditions, the quadrilateral elements eliminate the modeling bias
associated with the use of triangular elements, and quadrilaterals
give more accurate results for the same mesh size. If the surfaces are
highly warped, curved, or swept, triangular elements should be
used. Under extreme conditions quadrilateral elements will give
results that are considerably less accurate than triangular elements
for the sane mesh size. Quadrilateral elements should be kept as
nearly square as practicable, as the accuracy tends to deteriorate as
the aspect ratio of the quadrilateral increases. Triangular elements
should be kept as nearly equilateral as practicable, because the
accuracy tends to deteriorate as the angles become obtuse and as the
ratio of the longest to the shortest side increases.

## 1.3.6  Axisymmetric Shell Elements

The properties of axisymmetric shells can be specified with either of
two elements, the conical shell (CONEAX) or the toroidal ring
(TORDRG). However, these cannot be used together in the same
model. Also available for thick shells of revolution are the
axisymmetric solid elements (TRIARG, TRAPRG, TRIAAX, and TRAPAX) which
are described in the next section. Thin shell (TRSHL) modeling is
described in Section 1.3.12.

### 1.3.6.1  Conical Shell (CONEAX) Element

The properties of the conical shell element are assumed to be
symmetrical with respect to the axis of the shell. However, the loads
and deflections need not be axisymmetric, as they are expanded in
Fourier series with respect to the aximuthal coordinate. Due to
symmetry, the resulting load and deformation systems for different
harmonic orders are independent, a fact that results in a large time
saving when the use of the conical shell element is compared with an
equivalent model constructed from plate elements. Theoretical aspects
of the conical shell element are treated in Section 5.9 of the
Theoretical Manual.

The conical shell element may be combined with TRIAAX and TRAPAX
elements only. The existence of a conical shell problem is defined by
the AXIC card.  This card also indicates the number of harmonics
desired in the problem formulation. Only a limited number of bulk data
cards are allowed when using conical shell elements. The list of
allowable cards is given on the AXIC card description in Section
2.4.2.

The geometry of a problem using the conical shell element is described
with RINGAX cards instead of GRID cards. The RINGAX cards describe
concentric circles about the basic z-axis, with their locations given
by radii and z- coordinates as shown in Figure 1.3-7. The degrees of
freedom defined by each RINGAX card are the Fourier coefficients of
the motion with respect to angular position around the circle. For
example the radial motion, \\( u_r \\), at any angle, \\( \phi \\), is
described by the equation:

\begin{equation}
u\_r(\phi) = \sum^N\_{n=0} u^n\_r \cos(n\phi) + \sum^N\_{n=0} u^{n*}\_r \sin(n\phi)
\end{equation}

where \\( u_r^n \\) and \\( u\_r^{n\*} \\) are the Fourier
coefficients of radial motion for the n-harmonic. For calculation
purposes the series is limited to N harmonics as defined by the AXIC
card. The first sum in the above equation describes symmetric motion
with respect to the \\( \phi = 0 \\) plane. The second sum with the
"starred" (\*) superscripts describes the antisymmetric motion. Thus
each RINGAX data card will produce six times (N+l) degrees of freedom
for each series.

The selection of symmetric or antisymmetric solutions is controlled by
the AXISYM card in the Case Control Deck. For general loading
conditions, a combination of the symmetric and antisymmetric solutions
must be made, using the SYMCOM card in the Case Control Deck (Section
2.3 of User's Manual).

Since you are rarely interested in applying loads in terms of Fourier
harmonics and interpreting your data by manually performing the above
summations, NASTRAN is provided with special cards which automatically
perform these operations. The POINTAX card is used like a GRID card to
define physical points on the structure for loading and
output. Sections of the circle may be defined by a SECTAX card, which
defines a sector with two angles and a referenced RINGAX card. The
POINTAX and SECTAX cards define six degrees of freedom each. The
implied coordinate system for these points is a cylindrical system \\(
(r, \phi, z) \\) and their applied loads must be described in this
coordinate system. Since the displacements of these points are
dependent on the harmonic motions, they nay not be constrained in any
manner.

The conical shell element is connected to two RINGAX points with a
CCONEAX card. The properties of the conical shell element are
described on the PCONEAX card. The RINGAX points must be placed on the
neutral surface of the element and the points for stress calculation
must be given on the PCONEAX card relative to the neutral surface. Up
to fourteen angular positions around the element may be specified for
stress and force output. These values will be calculated midway
between the two connected rings.

The structure defined with RINGAX and CCONEAX cards must be
constrained in a special manner. All harmonics may be constrained for
a particular degree of freedom on a ring by using permanent
single-point constraints on the RINGAX cards. Specified harmonics of
each degree of freedom on a ring may be constrained with a SPCAX
card. This card is the same as the SPC card except that a harmonic
must be specified. The MPCAX, OMITAX, and SUPAX data cards correspond
the MPC, OMIT, and SUPORT data except that harmonics must be
specified. SPCADD and MPCADD cards may be used to combine constraint
sets in the usual manner.

The stiffness matrix includes five degrees of freedom per grid circle
per harmonic when transverse shear flexibility is included. Since the
rotation about the normal to the surface is not included, either the
fourth or the sixth degree of freedom (depending upon the situation)
must be constrained to zero when the angle between the meridional
generators of two adjacent elements is zero. When the transverse shear
flexibility is not included, only four independent degrees of freedom
are used, and the fourth and sixth degrees of freedom must be
constrained to zero for all rings. These constraints can be
conveniently specified on the RINGAX card.

The conical shell structure may be loaded in various
ways. Concentrated forces may be described by FORCE and MOMENT cards
applied to POINTAX points. Pressure loads may be input in the PRESAX
data card which defines an area bounded by two rings and two
angles. Temperature fields are described by a paired list of angles
and temperatures around a ring as required by the TEMPAX card. Direct
loads on the harmonics of a RINGAX point are given by the FORCEAX and
MOMAX card. Since the implied coordinate system is cylindrical, the
loads are given in the r, \\(\phi\\), and z directions. The value of a
harmonic load \\(F\_n\\) is the total load on the whole ring of radius
r. If a sinusoidal load per unit length of maximum value \\(a\_n\\) is
given, the value on the FORCEAX card must be

\begin{equation}
F\_n = 2 \alpha r a\_n
\end{equation} n = 0,

\begin{equation}
F\_n = \alpha r a\_n
\end{equation} n > 0.

Displacements of rings and forces in conical shell elements can be
requested in two ways:

1. The harmonic coefficients of displacements on a ring or forces in a
   conical element.
2. The displacements at specified points or the average value over a
   specified sector of a ring. The forces in the element at specified
   azimuths or average values over specified sectors of a conical
   element.

Harmonic output is requested by ring number for displacements and
conical shell element number for element forces. The number of
harmonics that will be output for any request is a constant for any
single execution. This number is controlled by the HARMONICS card in
the Case Control Deck (see Section 2.3).

The following element forces per unit of width are output either as
harmonic coefficients or at specified locations on request:

- Bending moments on the u and v faces
- Twisting moments
- Shearing forces on the u and v faces

The following element stresses are calculated at two specified points
on the cross-section of the element and output either as harmonic
coefficients or at specified locations on request:

- Normal stresses in u and v directions
- Shearing stress on the u face in the v direction
- Angle between the u-axis and the major principal axis
- Major and minor principal stresses
- Maximum shear stress

The manner in which the data cards for the CONEAX element (as well as
for the TRAPAX and TRIAAX elements) are processed is described in
Section 1.3.7.3.

### 1.3.6.2  Toroidal Ring (TORDRG) Element

The cylindrical coordinate system for the toroidal ring is implied by
the use of the toroidal element, and hence, no explicit definition is
required.  The toroidal element may use orthotropic materials. The
axes of orthotropy are assumed to coincide with the element coordinate
axes.

Deformation behavior of the toroidal element is described by five
degrees of freedom for each of the two grid rings which it
connects. The degrees of freedom in the implicit coordinate system
are:

1. \\( \bar{u} \\) - radial displacement
2. Not defined for toroidal element (must be constrained)
3. \\( \bar{w| \\) - axial displacement
4. \\( \prime{w} = \deriv{w}{e} \\) slope in e-direction
5. \\( \prime{u} = \deriv{u}{e} \\) strain in e-direction
6. \\( \pprime{w} = \dderiv{w}{e} \\) curvature in ze-plane

The displacements u and w are in the basic coordinate system, and
hence can be expressed in other local coordinate systems if
desired. However, the quantities \\( \prime{u} \\), \\( \prime{w} \\),
and \\( \pprime{w} \\) are always in the element coordinate system.

The toroidal ring element connectivity is defined with a CTORDRG card
and its properties with a PTORDRG card and, in the limit, this element
becomes a cap element (see Section 5.10 of the Theoretical
Manual). The integers 1 and 2 refer to the order of the connected grid
points on the CTORDRG card. The grid points must lie in the r-z plane
of the basic coordinate system and they must lie to the right of the
axis of symmetry. The angles à1 and à2 are the angles of curvature and
are defined as the angle measured in degrees from the axis of symmetry
to a line which is perpendicular to the tangent to the surface at grid
points 1 and 2 respectively. For conic rings \\( \alpha\_1 = \alpha\_2
\\) and for cylindrical rings \\( \alpha\_1 = \alpha\_2 = 90\degrees
\\). Toroidal elements may be connected to form closed figures in the
r-z plane, but slope discontinuities are not permitted at connection
points.

The following forces, evaluated at each end of the toroidal element,
are output on request:

- Radial force
- Axial force
- Meridional moment
- A generalized force which corresponds to the \\( \prime{w} \\)
  degree of freedom.
- A generalized force which corresponds to the \\( \pprime{w} \\)
  degree of freedom.

The first three forces are referenced to the global coordinate system
and the two generalized forces are referenced to the element
coordinate system. For a definition of the generalized forces see
Section 5.10 of the Theoretical Manual.

The following stresses, evaluated at both ends and the midspan of each
element, are output on request:

- Tangential membrane stress (Force per unit length)
- Circumferential membrane stress (Force per unit length)
- Tangential bending stress (Moment per unit length)
- Circumferential bending stress (Moment per unit length)
- Shearing stress (Force per unit length)
