# 2.4  BULK DATA DECK

   The primary NASTRAN input medium is the bulk data card. These cards are
used to define the structural model and various pools of data which may be
selected by Case Control at execution time.

   For large problems, the Bulk Data Deck may consist of several thousand
cards. In order to minimize the handling of large numbers of cards, provision
has been made in NASTRAN to store the bulk data on the Problem Tape, from
which it may be modified on subsequent runs. A User's Master File (Section
2.5) is also provided for the storage of Bulk Data Decks.

   For any cold start, the entire Bulk Data Deck must be submitted.
Thereafter, if the original run was checkpointed, the Bulk Data Deck exists on
the Problem Tape in sorted form where it may be modified and reused on
restart. On restart, the bulk data cards contained in the Bulk Data Deck are
added to the bulk data contained on the Old Problem Tape. Cards are removed
from the Old Problem Tape (or the User's Master File) by the use of a delete
card. Cards to be deleted are indicated by inserting a bulk data card with a /
in column one and the sorted bulk data sequence numbers in fields two and
three. All bulk data cards in the range of the sequence numbers in fields two
and three will be deleted. In the case where only a single card is deleted,
field three may be left blank.

   The Bulk Data Deck may be submitted with the cards in any order, as a sort
is performed prior to the execution of the Input File Processor. It should be
noted that the machine time to perform this is minimized for a deck that is
already sorted. The sort time for a badly sorted deck will become significant
for large decks. You may obtain a printed copy of either the unsorted or the
sorted bulk data by selection in the Case Control Deck. A sorted echo is
necessary in order to make modifications on a secondary execution using the
Problem Tape. This echo is automatically provided unless specifically
suppressed by you.

## 2.4.1  Format of Bulk Data Cards

   The bulk data cards can employ either the fixed-field format or the
free-field format. The free-field format bulk data cards are converted
internally by the program to appropriate fixed-field format cards.

   The fixed-field input format employs either 8-column or 16-column fields.
It is described in Section 2.4.1.1.

   The free-field input format relaxes the rigid 8-column field requirement.
It can be used in place of the fixed-field format in all cases of bulk data
cards that employ 8-column fields. It is described in Section 2.4.1.2.

   The free-field input format will be found useful not only by real-time
terminal users, but also by batch-job users who will find it helpful in
reducing errors due to mispunching of data in the wrong columns. In addition,
the free-field format offers the ability to automatically duplicate similar
bulk data cards with minor changes in one or more selected fields. Also,
several options are offered to make terminal keyboard data entry easier, to
allow you to execute only the NASTRAN Preface (Link 1), and to punch out
generated card images. The various options can be invoked at any time during a
free-field input session.

### 2.4.1.1  Fixed-Field Input

   The fixed-field input format is variable to the extent that any quantity
except the mnemonic can be punched anywhere within a specified 8 or 16-column
field. The normal card uses an 8-column field as indicated in the following
diagram.

Small Field Bulk Data Card

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1a   |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10a |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|    8   |   8   |   8   |   8   |   8   |   8   |   8   |   8   |    8  |  8  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

The mnemonic is punched in field 1. Fields 2-9 are for data items. The only
limitations on data items are that they must lie completely within the
designated field, have no imbedded blanks, and must be of the proper type,
that is, blank, integer, real, double precision, or BCD (see SEQGP and SEQEP
for exceptions). All real numbers, including zero, must contain a decimal
point. A blank will be interpreted as a real zero or integer zero as required.
Real numbers may be encoded in various ways. For example, the real number 7.0
may be encoded as 7.0, .7E1, 0.7+1, 70.-1, .70+1, etc. A double precision
number must contain both a decimal point and an exponent with the character D
such as 7.0D0. Double precision data values are only allowed in a few
situations, such as on the PARAM card. BCD data values consist of one to eight
alphanumeric characters, the first of which must be alphabetic.

   Normally field 10 is reserved for optional user identification. However, in
the case of continuation cards, field 10 (except column 73, which is not
referenced) is used in conjunction with field 1 of the continuation card as an
identifier and hence must contain a unique entry. The continuation card
contains the symbol + in column 1 followed by the same seven characters that
appeared in columns 74-80 of field 10 of the card that is being continued.
This allows the data to be submitted as an unsorted deck.

   The small field data card should be more than adequate for the kinds of
data normally associated with structural engineering problems. Since
abbreviated forms of floating point numbers are allowed, up to seven
significant decimal digits may be used in an eight-character field.
Occasionally, however, the input is generated by another computer program or
is available in a form where a wider field would be desirable. For this case,
the larger field format with a 16-character data field is provided. Each
logical card consists of two physical cards as indicated in the following
diagram.

Large Field Bulk Data Card

+--------+---------------+---------------+---------------+---------------+-----+
|   1a   |       2       |      3        |       4       |       5       | 10a |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|    8   |       16      |       16      |       16      |       16      |  8  |
+--------+---------------+---------------+---------------+---------------+-----+

+--------+---------------+---------------+---------------+---------------+-----+
|   1b   |       6       |      7        |       8       |       9       | 10b |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|    8   |       16      |       16      |       16      |       16      |  8  |
+--------+---------------+---------------+---------------+---------------+-----+

The large field card is denoted by placing the symbol * after the mnemonic in
field 1a and some unique character configuration in the last 7 columns of
field 10a. The second physical card contains the symbol * in column 1 followed
by the same seven characters that appeared after column 73 in field 10a of the
first card. The second card may in turn be used to point to a large or small
field continuation card, depending on whether the continuation card contains
the symbol * or the symbol + in column 1. The use of multiple and large field
cards is illustrated in the following examples.

Small Field Card with Small Field Continuation Card

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|TYPE    |       |       |       |       |       |       |       |       |QED12|
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+ED12   |       |       |       |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Large Field Card

+--------+---------------+---------------+---------------+---------------+-----+
|TYPE*   |               |               |               |               |QED13|
+--------+---------------+---------------+---------------+---------------+-----+
|*ED13   |               |               |               |               |     |
+--------+---------------+---------------+---------------+---------------+-----+

Large Field Card with Large Field Continuation Card

+--------+---------------+---------------+---------------+---------------+-----+
|TYPE*   |               |               |               |               |QED31|
+--------+---------------+---------------+---------------+---------------+-----+
|*ED31   |               |               |               |               |QED32|
+--------+---------------+---------------+---------------+---------------+-----+
|*ED32   |               |               |               |               |QED35|
+--------+---------------+---------------+---------------+---------------+-----+
|*ED35   |               |               |               |               |     |
+--------+---------------+---------------+---------------+---------------+-----+

Large Field Card Followed by a Small Field Continuation Card and a Large Field 
Continuation Card 

+--------+---------------+---------------+---------------+---------------+-----+
|TYPE*   |               |               |               |               |QD462|
+--------+---------------+---------------+---------------+---------------+-----+
|*D462   |               |               |               |               |QD421|
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+D421   |       |       |       |       |       |       |       |       |QD361|
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|*D361   |               |               |               |               |QD291|
+--------+---------------+---------------+---------------+---------------+-----+
|*D291   |               |               |               |               |     |
+--------+---------------+---------------+---------------+---------------+-----+

Small Field Card with Large Field Continuation Card

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|TYPE    |       |       |       |       |       |       |       |       |QD632|
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|*D632   |               |               |               |               |QD204|
+--------+---------------+---------------+---------------+---------------+-----+
|*D204   |               |               |               |               |     |
+--------+---------------+---------------+---------------+---------------+-----+

In the above examples, column 73 arbitrarily contains the symbol Q in all
cases where field 10 is used as a pointer. However, column 73 could have been
left blank or the same symbol used in column 1 of the following card could
have been used (that is, the symbols * or +).

### 2.4.1.2  Free-Field Input

   The free-field input format can be used to create only small field cards.
This capability is best understood by the following important rules and
program features:

   1. Free-field input is available only after a BEGIN BULK card is read, and
      is disabled automatically when ENDDATA is entered.

      In VAX and all UNIX machines, a free-field input card can have up to 94
      columns.

   2. Free-field input is activated by one or more commas (,) or an equal sign
      (=) in the first 10 columns of the input card.

   3. Data items must be separated with a comma, one or more blanks, or the
      combination of a comma and blanks.

      Logical choice is one comma only in first 10 columns of the input card.

   4. Integers and BCD words are limited to 8 digits or 8 characters. Real
      numbers can be up to 12 digits, including sign and decimal point.

   5. Duplication of fields from the preceding card is accomplished by coding
      an equal sign (=) in the appropriate field.

   6. Two equal signs (==) indicate duplication of all the trailing fields
      from the preceding card.

   7. Increment of a value from the previous input card is indicated by coding
      *(i), where i is the value of the increment (integer or floating point
      number) and * is the increment character. This feature is dependent on
      the field in the input card.

   8. Increment of a value from the previous input card to an ending value is
      indicated by coding %(E), where E is the ending value (integer or
      floating point number) in the last card to be generated, and % is the
      ending character. This feature is also field dependent.

   9. Repeated duplication is indicated by coding =(N), where N is the number
      of card images to be generated using the value of the increment on the
      preceding card (or current card) by *(i), or the computed incremental
      value on the preceding card by %(E). The last generated card is also
      displayed on the terminal screen if the prompt option (see rule 16
      below) has been turned on.

   10.  A field index and value can be coded by n)X, where n is the field
        index and X the value.

   11.  The symbol )+ is equivalent to 10)+, where 10 is the tenth field of
        the input card, which is normally the continuation ID field.

   12.  A right parenthesis ) in column 1 indicates the duplication of the
        tenth field of the preceding card into the first field of the current
        card being generated.

   13.  The continuation ID (in field 1 or 10) is automatically increased (by
        1) in the repeated-duplication operation. The ID must be in the form
        of +A-X, where A is one or more alphanumeric characters preceded by a
        plus and followed by a minus sign. X is an unsigned integer to be
        used as the initial value for the increment. A maximum of 8
        characters (including signs) is allowed, with no embedded blanks. An
        "=(1)" in the first input field is needed for single card
        duplication.

   14.  Data in field 10, not in the form of +A-X, is replaced by blanks
        during the repeated-duplication operation.

   15.  If a continuation card follows the parent card immediately in
        free-field input, the continuation ID's in both the parent and the
        child cards are optional. However, the child card must begin with a
        comma, for example

      CORD2C, 3  17 -2.9  1.0  0.0  3.6
      ,  5.2  1.0   -2.9

   16.  The ECHO card (described in the Case Control section, see Section
        2.3) can be input (or redefined) at any time during the free-field
        input session.

   17.  A new option, ECHO = LINK1, can be entered at any time to alter the
        NASTRAN execution sequence to that of link-one-only, and to skip
        BANDIT grid-point resequencing.

   18.  A prompt command -- PROMPT = ON, PROMPT = OFF, or PROMPT = YES
        (default) -- can be entered at any time during the free-field input
        session so that the computer will display, or not display, on the
        terminal screen a prompt symbol (either a ">" or "ENTER:") when it is
        ready to receive input data. The PROMPT = YES command will also
        display the generated card image on the screen in addition to the
        prompt symbol.

   19.  Floating point numbers in the forms of 12300., 1.23E+04, or 1.23+4
        are acceptable. Twelve digits can be used for maximum accuracy. For
        example, 1234567890.1 is more accurate than 0.123456D+10.

   20.  When not in free-field input mode, NASTRAN accepts only upper-case
        input cards. However, free-field input accepts both upper-case and
        lower-case letters. If lower-case letters are used in the free-field
        input, the first 8 columns of an input card must contain at least one
        lower-case letter. This triggers the free-field routine to convert
        all lower-case letters in that card to upper-case automatically.
        Otherwise, no such conversion takes place. (See Example 6 in Section
        2.4.1.2.1.)

   21.  Both BCD and EBCDIC character sets are acceptable. This is required
        for some computers (for example, IBM) with EBCDIC input cards.

   22.  The dollar sign ($) can be used freely as described elsewhere in
        Section 2.

   23.  Embedded blanks are not allowed in any double-character free-field
        input commands such as:

        =(   *(   %(   )+   ==

   24.  Embedded blanks are not allowed in field 10, which is sometimes used
        as a comment field.

   25.  A slash (/), with or without a separator of comma or blank, indicates
        that the current field is the same as the previous field, for example

        =(10),*(1),///   equals   =(10),*(1),*(1),*(1),*(1)

   26.  A "NASTRAN TITLEOPT = -2" card is recommended to be the very first
        line of input for all terminal users executing only LINK1 (see rule
        15 above). It suppresses the printout of the NASTRAN title pages on
        the screen. This card is required for UNIVAC terminal users executing
        LINK1; it also reassigns the alternate print file (the log-message
        file) to avoid system crashing.

   A stand-alone version of NASTRAN free-field input is available to you by
executing NASTRAN LINKFF. It has all of the features described above except
for the following changes:

   1. The ECHO command is not available in this version.

   2. Two additional commands are available only in this version. They are:

      a.SCALE/8 or SCALE/10 - to display a scale based on 8-column or
        10-column format on the screen to aid in input spacing.

      b.CANCEL = n - to cancel n previously generated cards.

   3. The punch option and catalog file (to save generated card images) are
      set at the beginning of this version.

#### 2.4.1.2.1  Free-Field Input Examples

The following examples illustrate the use of free-field input.

Example 1
---------

   GRID, 2, 3, 1.0 2.0,, 4,316

   =, *(1), =, *(.2), == $

   =(3)

   The above free-field cards will generate the following bulk data cards in
NASTRAN 8-column field format:

    1       2       3       4       5       6       7       8       9      10
--------++++++++--------++++++++--------++++++++--------++++++++--------++++++++
GRID    2       3       1.0     2.0             4       316
GRID    3       3       1.2     2.0             4       316
GRID    4       3       1.4     2.0             4       316
GRID    5       3       1.6     2.0             4       316
GRID    6       3       1.8     2.0             4       316

Example 2
---------

   grid,2,3,1.0,2.0,,4,316

   =(4),*(1),=,%(1.8),==

   The above cards will generate the same bulk data cards as in Example 1.

Example 3
---------

   Grid, 2 3 1.0 2.0, 7) 4, 316

This example will generate only one card. This will be the same as the
first card in Example 1.

Example 4
---------
   Tabled3,62, 126.9, 30.0 10)+abc

   ),  1.23e+4,  5.67+8, 1234567. endt

This example will generate the following bulk data cards:

    1       2       3       4       5       6       7       8       9      10
--------++++++++--------++++++++--------++++++++--------++++++++--------++++++++

TABLED3 62      126.9   30.0                                            +ABC

+ABC    1.23E+4 5.67+8  1234567.ENDT

Example 5
---------
   taBLed3, 62 126.9 30.0 )+aBc

This example will generate only one card. This will be the same as the
first card in Example 4.

Example 6
---------
   This is only a test

   THIS IS only a test

   This, is only a test

The different results of the above three (3) input lines are shown by the
following generated card images:

    1       2       3       4       5       6       7       8       9      10
--------++++++++--------++++++++--------++++++++--------++++++++--------++++++++

THIS IS ONLY A TEST

THIS IS only a test

THIS    IS      ONLY    A       TEST

Example 7
---------
   PBAR, 3, 4, 5.0 , 6.0, )+ABC-1

   = , *(1), =, *(2.)  ==

   =(2)

   +ABC-1, 7.7  8.8  9  )+DEF-22

   =(3),==

   This example will generate the following eight (8) cards with continuation
ID fields automatically increased by 1.

    1       2       3       4       5       6       7       8       9      10
--------++++++++--------++++++++--------++++++++--------++++++++--------++++++++

PBAR    3       4       5.0     6.0                                     +ABC-1
PBAR    4       4       7.0     6.0                                     +ABC-2
PBAR    5       4       9.0     6.0                                     +ABC-3
PBAR    6       4       11.0    6.0                                     +ABC-4
+ABC-1  7.7     8.8     9                                               +DEF-22
+ABC-2  7.7     8.8     9                                               +DEF-23
+ABC-3  7.7     8.8     9                                               +DEF-24
+ABC-4  7.7     8.8     9                                               +DEF-25

Example 8
---------

   CQUAD2,    101     1      11   12  16  15

   CQUAD2,    102     1      12   13  17  16

   CQUAD2,    103     1      13   14  18  17

   This example shows the combination of free-field and tabulation input. The
requirement of 8 columns per field does not apply here.

Example 9
---------
   This example lists the input data using free-field bulk data cards used in
NASTRAN Demonstration Problem No. D01-06-2A. It gives the same sorted input
data as NASTRAN Demonstration Problem No. D01-06-1A, which uses the standard
fixed-field bulk data cards.

::
  ID D01062A,NASTRAN
  APP     DISP
  SOL     1,1
  TIME    5
  CEND
  TITLE = SOLID DISC WITH RADIALLY VARYING THERMAL LOAD (FREE-FIELD)
  SUBTITLE = NASTRAN DEMONSTRATION PROBLEM NO. D01-06-2A
  LABEL = TRAPEZOIDAL RING ELEMENTS
  ECHO  = BOTH
  SPC   = 16
  TEMPERATURE(LOAD) = 16
      OUTPUT
      SET 1 = 1,3,5,7,9,11,13,15,17,19,21,23,25,26
      DISP  = 1
      ELSTRESS =  ALL
  BEGIN BULK
  CTRAPRG, 1,1,3,4,2,.0,12
  =(11), *(1) *(2),///, ==
  GRDSET, 8)2456
  GRID,1,,.0
  =(3),*(2),,*(.005)
  GRID,2,,.0,,.01
  =(3),*(2),,*(.005),==
  GRID,9,,.02
  =(8),*(2),,%(.10)
  GRID,10,,.02,,.01
  =(8),*(2),,%(.10),==
  MAT1,12,1.0+7,,.3,.2587-3,1.0-7,.0
  SPC,16,1,13,.0,2,1,.0
  TEMP,16,1,100.,2,100.,3,99.75
  =,=,4,99.75,5,99.0,6,99.0
  =,=,7,97.75,8,97.75,9,96.0
  =,=,10,96.0,11,91.0,12,91.0
  =,=,13,84.0,14,84.0,15,75.0
  =,=,16,75.0,17,64.0,18,64.0
  =,=,19,51.0,20,51.0,21,36.0
  =,=,22,36.0,23,19.0,24,19.0
  =,=,25,.0,26,.0
  ENDDATA

## 2.4.2  Bulk Data Card Descriptions

The detailed descriptions of the bulk data cards are contained in this
section in alphabetical order. For details pertaining to the use of each card
and for a discussion of the cards in functional groups, you are referred to
Section 1, Structural Modeling. Small field examples are given for each card
along with a description of the contents of each field. In the Format and
Example section of each card description, both a symbolic card format
description and an example of an actual card are shown. Literal constants are
shown in the card format section enclosed in quotes (for example, "0"). Fields
that are required to be blank are indicated in the card format section by a
blank box.

The Input File Processor will produce error messages for any cards that do
not have the proper format or that contain illegal data.

Continuation cards need not be present unless they contain required data.
In the case of multiple continuation cards, the intermediate cards must be
present (even though fields 2-9 are blank), if one of the following cards
contains data in fields 2-9. In addition, a double field format requires at
least two cards (or subsequent multiples of two) so that 10 data fields are
included. Thus, one or more double field cards may contain no data.

# $ - Comment

## Description

Comment cards are for user convenience in inserting commentary material into
the unsorted echo of the input Bulk Data Deck. The $ card is otherwise ignored
by the program. These cards will not appear in a sorted echo nor will they
exist on the New Problem Tape.

## Format and Example

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|$       followed  by any legitimate characters in card columns 2-80     |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|$       |THIS IS|A REMARK (*,'$$|-+/    |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

# / - Delete

## Description

Delete cards are used to remove cards from either the Old Problem Tape on
restart or the User's Master File.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|/       |   K1  |   K2  |       |       |       |       |       |       |     |
|/       |   4   |       |       |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field      Contents
-----      --------
K1         Sorted sequence number of first card in sequence to be removed.

K2         Sorted sequence number of last card in sequence to be removed.

## Remarks

1. The delete card causes bulk data cards having sort sequence numbers K1
   through K2 to be removed from the Bulk Data Deck.

2. If K2 is blank, only card K1 is removed from the Bulk Data Deck.

3. If neither an Old Problem Tape nor a User's Master File is used in the
   current execution, the delete cards are ignored.

# ADUMi - Dummy Element Attributes

## Description
Defines attributes of the dummy elements (1 <= i <= 9).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|ADUMi   |  NG   |  NC   |  NP   |  ND   |       |       |       |       |     |
|ADUM2   |   8   |   2   |   1   |   3   |       |       |       |       |     |

Field      Contents
-----      --------
NG         Number of grid points connected by DUMi dummy element (Integer >
           0).

NC         Number of additional entries on CDUMi connection card (Integer >=
           0).

NP         Number of additional entries on PDUMi property card (Integer >=
           0).

ND         Number of displacement components at each grid point used in
           generation of differential stiffness matrix (Integer 3 or 6).

AEFACT - Aerodynamic Spanwise Divisions
=======================================

## Description

Used to specify box division points for flutter analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AEFACT  |  SID  |  D1   |  D2   |  D3   |  D4   |  D5   |  D6   |  D7   |ABC  |
|AEFACT  |  97   |  .3   |  .7   |  1.0  |       |       |       |       |     |
|+BC     |  D8   |  D9   |  etc. |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AEFACT  | SID   | D1    | THRU  | DND   | ND    | DMID  |       |       |     |
|AEFACT  | 201   |.200   | THRU  | .100  | 11    |.133333|       |       |     |

Field      Contents
-----      --------
SID        Set identification number (unique Integer > 0).

Di         Division point (Real).

## Remarks

1. These factors must be selected by a CAEROi or PAEROi data card to be used
   by NASTRAN.

2. Imbedded blank fields are forbidden.

3. If used to specify box division points, note that there is one more
   division point than the number of boxes.

4. For the alternate form, ND must be greater than 1. Dmid must lie between D1
   and DND, otherwise Dmid will be set to (D1 + DND)/2. Then

        D (D  -D   )(ND-i) + D  (D   -D )(i-1)
         1  ND  mid           ND  mid  1
   D  = --------------------------------------- i = 1,2,...,ND
    i   (D  -D   )(ND-i) + (D   -D )(i-1)
          ND  mid            mid  1

   The use of Dmid (middle point selection) allows unequal spacing of the
   points. Dmid = 2D1DND/(D1+DND) gives equal values to increments of the
   reciprocal of D1.

================================
AERO - Aerodynamic Physical Data
================================

## Description

Gives basic aerodynamic parameters.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AERO    |ACSID  |VEL    |REFC   |RHOREF |SYMXZ  |SYMXY  |       |       |     |
|AERO    |3      |1.3+4  |100.   |1.-5   |       |1      |       |       |     |

Field      Contents
-----      --------
ACSID      Aerodynamic coordinate system identification (Integer >= 0). See
           Remark 2.

VEL        Velocity (Real).

REFC       Reference length (for reduced frequency) (Real).

RHOREF     Reference density (Real).

SYMXZ      Symmetry key for aero coordinate x-z plane (Integer) (+1 for
           symmetry, 0 for no symmetry, -1 for anti-symmetry).

SYMXY      Symmetry key for aero coordinate x-y plane can be used to simulate
           ground effects (Integer), same code as SYMXZ.

## Remarks

1. This card is required for aerodynamic response problems. Only one AERO card
   is allowed.

2. The ACSID must be a rectangular coordinate system. Flow is in the positive
   x direction.

ASET - Selected Coordinates
===========================

## Description

Defines coordinates (degrees of freedom) to be placed in the analysis set.
Used to define the number of independent degrees of freedom.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
| ASET   |   ID  |   C   |   ID  |   C   |   ID  |   C   |   ID  |   C   |     |
| ASET   |   16  |   2   |   23  | 3516  |       |       |   1   |   4   |     |

Field      Contents
-----      --------
ID         Grid or scalar point identification number (Integer > 0).

C          Component number, zero or blank for scalar points, any unique
           combination of the digits 1 - 6 for grid points.

## Remarks

1. Coordinates specified on ASET cards may not be specified on OMIT, OMIT1,
   ASET1, SPC, or SPC1 cards, nor may they appear as dependent coordinates in
   multipoint constraint relations (MPC), nor as rigid elements (CRIGD1,
   CRIGD2, CRIGD3, CRIGDR), nor as permanent single-point constraints on a
   GRID card.

2. As many as 24 coordinates may be placed in the analysis set by a single
   card.

3. When ASET and/or ASET1 cards are present, all degrees of freedom not
   otherwise constrained or referenced on a SUPORT card will be placed in the
   O-set.

4. ASET or OMIT data are not recommended for use in heat transfer analysis
   with radiation effects.

============================
ASET1 - Selected Coordinates
============================

## Description

Defines coordinates (degrees of freedom) to be placed in the analysis set.
Used to define the number of independent degrees of freedom.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
| ASET1  |   C   |   G   |   G   |   G   |   G   |   G   |   G   |   G   | abc |
| ASET1  |  345  |   2   |   1   |   3   |   10  |   9   |   6   |   5   | ABC |
|  +bc   |   G   |   G   |   G   |  etc. |       |       |       |       |     |
|  +BC   |   7   |   8   |       |       |       |       |       |       |     |

## Alternate Form


|--------|-------+-------|-------|-------+-------|-------|-------|-------|-----|
| ASET1  |   C   |  ID1  |"THRU" |  ID2  |       |       |       |       |     |
| ASET1  |123456 |   7   |THRU   |  109  |       |       |       |       |     |

Field      Contents
-----      --------
C          Component number (any unique combination of the digits 1 - 6 [with
           no imbedded blanks] when point identification numbers are grid
           points; must be null or zero if point identification numbers are
           scalar points).

G, ID1, ID2Grid or scalar point identification numbers (Integer > 0, ID1 <
           ID2).

## Remarks

1. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multipoint constraint relation (MPC card) nor as a degree
   of freedom on a rigid element (CRIGD1, CRIGD2, CRIGD3, CRIGDR), nor may it
   be referenced on an SPC, SPC1, OMIT, OMIT1, or ASET card, nor on a GRID
   card as permanent single-point constraints.

2. When ASET and/or ASET1 cards are present, all degrees of freedom not
   otherwise constrained or referenced on a SUPORT card will be placed in the
   O-set.

3. If the alternate form is used, all of the grid (or scalar) points ID1
   through ID2 are assumed.

4. ASET or OMIT data are not recommended for use in heat transfer analysis
   with radiation effects.

AXIC - Axisymmetric Problem Flag
================================

## Description

Defines the existence of a model containing CCONEAX, CTRAPAX, or CTRIAAX
elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AXIC    |   H   |       |       |       |       |       |       |       |     |
|AXIC    |   15  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
H          Highest harmonic defined for the problem (0 <= Integer <= 998).

## Remarks

1. Only one (1) AXIC card is allowed. When the AXIC card is present, most
   other cards are not allowed. The types which are allowed with the AXIC card
   are listed below.

   CCONEAX           GRAV          RLOAD1
   CTRAPAX           LOAD          RLOAD2
   CTRIAAX           MAT1          SECTAX
   DAREA             MATT1         SPCADD
   DELAY             MOMAX         SPCAX
   DLOAD             MOMENT        SUPAX
   DMI               MPCADD        TABDMP1
   DMIG              MPCAX         TABLED1
   DPHASE            NOLIN1        TABLED2
   DSFACT            NOLIN2        TABLED3
   EIGB              NOLIN3        TABLED4
   EIGC              NOLIN4        TABLEM1
   EIGP              OMITAX        TABLEM2
   EIGR              PARAM         TABLEM3
   EPOINT            PCONEAX       TABLEM4
   FORCE             POINTAX       TEMPAX
   FORCEAX           PRESAX        TF
   FREQ              PTRAPAX       TIC
   FREQ1             PTRIAAX       TLOAD1
   FREQ2             RFORCE        TLOAD2
                     RINGAX        TSTEP

2. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

3. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.

4. Machine bit limit may be exceeded on 32-bit word machines if H is greater
   than 16.


# AXIF - Fluid Related Axisymmetric Parameters

## Description

Defines basic parameters and the existence of an axisymmetric fluid analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AXIF    |  CID  |   G   | DRHO  |  DB   | NOSYM |   F   |       |       |abc  |
|AXIF    |   2   | 32.2  | 0.12  | 2.5+5 |  YES  |       |       |       |CARD1|

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+bc     |   N1  |  N2   |  N3   |  N4   |  N5   |  N6   |  N7   |  N8   |def  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+ARD1   |   1   |   2   |   3   |       |   4   |       |   7   |  10   |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
                                 etc. 

Alternate Form of Continuation Card:

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+bc     |   N1  |"THRU" |  Ni   |       |       |       |       |       |def  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+ARD1   |   0   | THRU  |  10   |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
                                 etc. 

Alternate Form of Continuation Card:

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+bc     |   N1  |"THRU" |  Ni   |"STEP" |   NS  |       |       |       |def  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+ARD1   |   0   | THRU  |   9   | STEP  |   3   |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
                                 etc. 

Field      Contents
-----      --------
CID        Fluid coordinate system identification number (Integer > 0).

G          Value of gravity for fluid elements in axial direction (Real).

DRHO       Default mass density for fluid elements (Real > 0.0 or blank).

DB         Default bulk modulus for fluid elements (Real).

NOSYM      Request for nonsymmetric (sine) terms of series (BCD: YES or NO).

F          Flag specifying harmonics (Blank - harmonic specified, or BCD
           NONE).

Nn         Harmonic numbers for solution, an increasing sequence of integers.
           On the standard continuation card blanks are ignored. On the
           alternate form continuation cards, THRU implies all numbers
           including upper and lower integer (Blank, or integer, 0 <= Nn <
           100, or BCD: THRU or STEP).

NS         Every NSth step of the harmonic numbers specified in the THRU
           range is used for solution (Integer if field 5 is STEP, Ni =
           I*NS+N1 where I is an integer).

## Remarks

1. Only one (1) AXIF card is allowed.

2. CID must reference a cylindrical or spherical coordinate system.

3. Positive gravity (+G) implies that the direction of free fall is in the -Z
   direction of the fluid coordinate system.

4. The DRHO value replaces blank values of RHO on the FSLIST, BDYLIST, and
   CFLUIDi cards.

5. The DB value replaces blank values of B on the CFLUIDi cards. If the
   CFLUIDi entry is blank and DB is zero or blank, the fluid is
   incompressible.

6. If NOSYM = YES, both sine and cosine terms are specified. If NOSYM = NO,
   only cosine terms are specified.

7. If F = NONE, no harmonics are specified, no fluid elements are necessary,
   and no continuation cards may be present.

Example
-------

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AXIF    |  100  | -386.0|       |  0.0  |  NO   |       |       |       |+1   |
|+1      |   0   | THRU  |  50   | STEP  |   5   |       |       |       |+2   |
|+2      |   52  |       |       |       |       |       |       |       |+3   |
|+3      |   54  | THRU  |  57   |       |       |       |       |       |+4   |
|+4      |   61  | THRU  |  65   |       |       |       |       |       |+5   |
|+5      |   68  |       |  71   |       |  72   |  75   |       |       |+6   |
|+6      |   81  |  92   |       |       |       |       |       |       |END  |


# AXSLOT - Axisymmetric Slot Analysis Parameter

## Description

Defines the harmonic index and the default values for acoustic analysis cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|AXSLOT  |RHOD   |  BD   |   N   |   WD  |   MD  |       |       |       |     |
|AXSLOT  | 0.003 | 1.5+2 |   3   |  0.75 |   6   |       |       |       |     |

Field      Contents
-----      --------
RHOD       Default density of fluid-mass/volume (Real not equal 0.0 or
           blank).

BD         Default bulk modulus of fluid = (force/volume ratio change) (Real
           >= 0.0 or blank).

N          Harmonic index number (Integer >= 0).

WD         Default slot width (Real >= 0.0 or blank).

MD         Default number of slots (Integer >= 0 or blank).

## Remarks

1. No more than one AXSLOT card is permitted.

2. The default values are used on the GRIDS, SLBDY, CAXIFi, and CSLOTi data
   cards and must be nonzero as noted if these cards use the default.

3. The harmonic index number N must be entered on this card.

4. If the number of slots, M, is different in different regions of the cavity,
   this fact may be indicated on the CSLOTi and SLBDY cards. If the number of
   slots is zero, no matrices for CSLOTi elements are generated.

5. A zero entry for bulk modulus is treated as if the fluid were
   incompressible.


# BAROR - Simple Beam Orientation Default

## Description

Defines default values for fields 3 and 6-9 of the CBAR card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BAROR   |       |  PID  |       |       |X1,GO  |   X2  |   X3  |   F   |     |
|BAROR   |       |   39  |       |       |  0.6  |  2.9  | -5.87 |   1   |     |

Field      Contents
-----      --------
PID        Identification number of PBAR property card (Integer > 0 or
           blank).

X1, X2, X3 Vector components measured in displacement coordinate system at GA
           to determine (with the vector from end A to end B) the orientation
           of the element coordinate system for the bar element (Real or
           blank; see below).

GO         Grid point identification number (Integer > 0; see below).

F          Flag to specify the nature of fields 6-8 as follows:

|       |   6   |   7   |   8   |
|-------|-------|-------|-------|
| F = 1 |   X1  |   X2  |   X3  |
| F = 2 |   GO  | blank | blank |

## Remarks

1. The contents of fields on this card will be assumed for any CBAR card whose
   corresponding fields are blank.

2. Only one BAROR card may appear in your Bulk Data Deck.

3. For an explanation of bar element geometry, see Section 1.3.2.

4. If F = 2, GO must be given even though it may be overridden on every CBAR
   card.

5. (Pre-1989 NASTRAN version) If F field is to be specified, at least one
   other field must be non-zero.

6. Since 1990 NASTRAN version, the F field is no longer required on a CBAR
   card. This makes the use of the BAROR card unnecessary.

BDYC - Combination of Substructure Boundary Sets

## Description

Defines a combination of boundary sets by basic substructure to define a set
of grid points and components which may be used in a CREDUCE, MREDUCE, or
REDUCE operation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BDYC    |   ID  | NAME1 |  SID1 | NAME2 |  SID2 | NAME3 |  SID3 |       |ghi  |
|BDYC    |  157  |WINGRT |   7   | MIDWG |   15  |FUSELAG|   32  |       |GHI  |

+--------+-------+-------+-------+-------------------------------+-------+-----+
|+hi     |       | NAMEi |  SIDi |                 etc.          |       |jkl  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+HI     |       | POD1  | 175   |WINGRT |   15  | CABIN |  16   |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field      Contents
-----      --------
ID         Identification number of combination boundary set (Integer > 0).

NAMEi      Name of basic substructure which contains the grid points defined
           by boundary set SIDi (BCD).

SIDi       Identification number of the boundary set associated with basic
           substructure NAMEi (Integer > 0).

## Remarks

1. Boundary sets must be selected in the Substructure Control Deck (BOUNDARY =
   ID) to be used by NASTRAN. Note that "BOUNDARY" is a subcommand of the
   substructure CREDUCE, MREDUCE, and REDUCE commands.

2. The same substructure name may appear more than once per set.

3. The SIDi numbers need not be unique. The same number could appear for
   different component structures.

4. The SIDi numbers reference the set IDs of BDYS and BDYS1 cards.

5. The ID number must be unique with respect to all other BDYC data cards.

6. After two or more basic substructures are combined, the connected degrees
   of freedom are actually the same and may be referenced with any one of the
   substructure names. Redundant specification is allowed.


# BDYLIST - Fluid Boundary List

## Description

Defines the boundary between a fluid and a structure.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BDYLIST | RHO   | IDF1  | IDF2  | IDF3  | IDF4  | IDF5  | IDF6  | IDF7  |abc  |
|+bc     | IDF8  |  etc. |       |       |       |       |       |       |def  |
|BDYLIST | .037  | 432   | 325   | 416   | 203   | 256   | 175   | 153   |345A |
|+45A    |  101  |  105  | AXIS  |       |       |       |       |       |     |

Field      Contents
-----      --------
RHO        Fluid mass density at boundary (Real >= 0.0 or blank. Default on
           AXIF card is used if blank.)

IDFi       Identification number of a RINGFL point (Integer > 0 or BCD. AXIS
           may be first and/or last entry on the logical card.)

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. Each logical card defines a boundary if RHO is not equal to 0.0. The order
   of the points must be sequential with the fluid on the right with respect
   to the direction of travel.

3. The BCD word "AXIS" defines an intersection with the polar axis of the
   fluid coordinate system.

4. There may be as many BDYLIST cards as required. If the fluid density varies
   along the boundary there must be one BDYLIST card for each interval between
   fluid points.

5. The BDYLIST card is not required and should not be used to specify a rigid
   boundary where structural points are not defined. Such a boundary is
   automatically implied by the omission of a BDYLIST.

6. If RHO is 0.0, no boundary matrix terms will be generated to connect the
   GRIDB points to the fluid. This option is a convenience for structural
   plotting purposes. GRIDB points may be located on a fluid ring (RINGFL)
   only if the rings are included in a BDYLIST.

# BDYS - Boundary Set Definition

## Description

The BDYS card is used to define a boundary set of grid points and degrees of
freedom for a basic substructure. The boundary set is used in the substructure
REDUCE, CREDUCE, and MREDUCE operations.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BDYS    |  SID  |  G1   |   C1  |  G2   |  C2   |   G3  |  C3   |       |     |
|BDYS    |   7   |  13   |123456 |  15   |  123  |   17  |123456 |       |     |

Field      Contents
-----      --------
SID        Identification number of BDYS set (Integer > 0).

Gi         Grid or scalar point identification number of a basic substructure
           (Integer > 0).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

## Remarks

1. The set of boundary points defines the degrees of freedom which are to be
   retained in the matrices after the substructure REDUCE, CREDUCE or MREDUCE
   operation has been performed. An alternate input format is provided by the
   BDYS1 card.

2. The SID need not be unique.

3. The BDYS card must be referenced by the BDYC card in order to attach the
   basic substructure name to the boundary set specified on the BDYS card.
   Note that the same BDYS boundary set may be attached to more than one basic
   substructure name.


# BDYS1 - Boundary Set Definition

## Description

The BDYS1 card is used to define a boundary set of grid points and degrees of
freedom for a basic substructure. The boundary set is used in the substructure
REDUCE, CREDUCE, and MREDUCE operations.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BDYS1   | SID   |  C    |   G1  |  G2   |  G3   |  G4   |  G5   |   G6  |abc  |
| +bc    |  G7   |  G8   |      etc.     |   GN  |       |       |       |     |
|BDYS1   |  15   |123456 |  275  |  276  | THRU  |  457  |  589  |  102  |ABC  |
| +BC    |  103  |  105  |       |       | 1275  |       |       |       |     |

Field      Contents
-----      --------
SID        Identification number of BDYS1 set (Integer > 0).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

Gi         Grid or scalar point identification number of a basic substructure
           (Integer > 0).

## Remarks

1. The set of boundary points defines the degrees of freedom which are to be
   retained in the matrices after the substructure REDUCE, CREDUCE or MREDUCE
   operation has been performed. An alternate format is provided by the BDYS
   card.

2. The THRU may appear in any field other than 2 and 9.

3. The SID need not be unique.

4. The BDYS1 card must be referenced by the BDYC card in order to attach the
   basic substructure name to the boundary set specified on the BDYS card.
   Note that the same BDYS boundary set may be attached to more than one basic
   substructure name.


# BFIELD - Magnetic Induction Output

## Description

Specifies coordinate system for magnetic induction output.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BFIELD  |  CID  | EID1  |  EID2 |  EID3 |  EID4 |  EID5 |  EID6 |  EID7 |     |
|BFIELD  |   3   |  12   |   5   |   6   |       |       |       |       |     |

First Alternate Form:
---------------------

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BFIELD  |  CID  |  EID1 |"THRU" |  EID2 |       |       |       |       |     |
|BFIELD  |   5   |   8   | THRU  |   27  |       |       |       |       |     |

Second Alternate Form:
----------------------

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|BFIELD  |  CID  |  -1   |       |       |       |       |       |       |     |
|BFIELD  |   7   |  -1   |       |       |       |       |       |       |     |

Field      Contents
-----      --------
CID        Coordinate system identification number (Integer > 0 or blank).

EIDi       Element identification numbers of those elements whose magnetic
           induction are to be output in coordinate system CID (Integer > 0).

## Remarks

1. The magnetic induction of any element not specified on a BFIELD card will
   be computed in the basic coordinate system. Therefore, no BFIELD cards are
   necessary if CID = 0 for all elements.

2. If the first alternate form of the card is used, all element identification
   numbers between EID1 and EID2 need not exist, but sufficient core must be
   available for 2(EID2 - EID1 + 1) words.

3. The second alternate form of the card implies that the magnetic induction
   values of all elements in the problem will be computed in coordinate system
   CID.


# CAERO1 - Aerodynamic Panel Element Connection

## Description

Defines an aerodynamic macro element (panel) in terms of two leading edge
locations and side chords for Doublet-Lattice Theory.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAERO1  | EID   | PID   | CP    | NSPAN | NCHORD| LSPAN | LCHORD| IGID  |ABC  |
| +BC    |  X1   |  Y1   |   Z1  |  X12  | X4    | Y4    | Z4    | X43   |     |
|CAERO1  | 1000  | 1     |       | 3     |       |       | 2     | 1     |ABC  |
| +BC    |  0.0  |  0.0  |  0.0  |  1.0  | 0.2   | 1.0   | 0.0   | 0.8   |     |

Field      Contents
-----      --------
EID        Element identification number (unique Integer > 0).

PID        Identification number of property card (Integer > 0) to specify
           associated bodies.

CP         Coordinate system for locating points 1 and 4 (Integer >= 0).

NSPAN      Number of spanwise boxes; if a positive value is given, equal
           divisions are assumed; if zero or blank, a list of division points
           follows (Integer >= 0).

NCHORD     Number of chordwise boxes (same rule as for NSPAN).

LSPAN      ID of an AEFACT data card containing a list of division points for
           spanwise boxes. Used only if field 5 is zero or blank (Integer > 0
           if NSPAN is zero or blank).

LCHORD     ID of an AEFACT data card containing a list of division points for
           chordwise boxes. Used only if field 6 is zero or blank (Integer >
           0 if NCHORD is zero or blank).

IGID       Interference group identification (aerodynamic elements with
           different IGID's are uncoupled) (Integer > 0).

X1,Y1,Z1;X4,Y4,Z4  Location of points 1 and 4, in coordinate system CP (Real).

X12; X43   Edge chord length (in aerodynamic coordinate system) (Real >= 0,
           and not both zero).

## Remarks

                       Z                                      Y
                        elem                                   elem
                    1  *-------+--------+--------*-----------
                       | 1000  | 1003   | 1006   | 4
                       +-------+--------+--------+
                       | 1001  | 1004   | 1007   |
                       +-------+--------+--------+
                       |       |        |        |
                       |       |        |        |
                       | 1002  | 1005   | 1008   |
                       |       |        |        |
                       |       |        |        |
                       |       |        |        |
                       |       |        |        |
                    2  *-------+--------+--------* 3
                       |
                       |
                       |
                       |
                         X     = X
                          aero    elem


1. The boxes are numbered sequentially, beginning with EID. You should be
   careful to ensure that all box numbers are unique, and different from
   structural grid ID's.

2. The number of division points is one greater than the number of boxes.
   Thus, if NSPAN = 3, the division points are 0.0, 0.333, 0.667, 1.000. If
   you supply division points, the first and last points need not be 0. and 1.
   (in which the corners of the panel would not be at the reference points).

3. A triangular element is formed if X12 or X43 = 0.

4. The element coordinate system (right-handed) is shown in the sketch.

5. The continuation card is required.


# CAERO2 - Aerodynamic Body Connection

## Description

Defines an aerodynamic body for Doublet-Lattice aerodynamics.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAERO2  |EID    |PID    |CP     |NSB    |NINT   |LSB    |LINT   |IGID   |ABC  |
|+BC     |X1     |Y1     |Z1     |X12    |       |       |       |       |     |
|CAERO2  |1500   |2      |100    |       |4      |99     |       |1      |abc  |
|+bc     |-1.0   |100.   |-30.   |175.   |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number  (Integer > 0).

PID        Property identification number (Integer > 0).

CP         Coordinate system for locating point 1 (Integer >= 0).

NSB        Number of slender body elements; if a positive number is given,
           NSB equal divisions are assumed; if zero or blank, see field 7 for
           a list of divisions (Integer >= 0).

NINT       Number of interference elements; if a positive number is given,
           NINT equal divisions are assumed; if zero or blank, see field 8
           for a list of divisions (Integer >= 0).

LSB        ID of an AEFACT data card for slender body division points; used
           only if field 5 is zero or blank (Integer >= 0).

LINT       ID of an AEFACT data card containing a list of division points for
           interference elements used only if field 6 is zero or blank
           (Integer >= 0).

IGID       Interference group identification (aerodynamic elements with
           different IGID's are uncoupled) (Integer > 0).

X1,Y1,Z1   Location of point 1 in coordinate system CP (Real).

X12        Length of body in the x-direction of the aerodynamic coordinate
           system (Real > 0).

## Remarks

1. Point 1 is the leading point of the body.

2. All CAERO1 (panels) and CAERO2 (bodies) in the same group (IGID) will have
   aerodynamic interaction.

3. Interference elements are optional, but if used at least one element is
   required for each aerodynamic body specified by this card.

4. Element identification numbers on the aerodynamic bodies must have the
   following sequence:

   a. CAERO1 panels first (lowest number)
   b. Z-bodies (see PAERO2 ORIENTation flag)
   c. ZY-bodies
   d. Y-bodies (highest number)

   and they must be unique with respect to all structural grid ID's.

5. The total number of interference bodies associated with a panel is limited
   to six.

6. At least two slender body elements are required for every aerodynamic body
   specified by this card.


# CAERO3 - Aerodynamic Mach Box Surface Connection

## Description

Defines the aerodynamic edges of a Mach Box lifting surface. If no cranks are
present, this card defines the aerodynamic Mach Box lifting surface.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAERO3  | EID   | PID   |  CP   | LISTW | LISTC1| LISTC2|       |       |ABC  |
|+BC     | X1    |   Y1  |   Z1  |  X12  |  X4   |  Y4   |  Z4   |  X43  |     |
|CAERO3  | 2000  | 2001  |   0   |  22   |   33  |       |       |       |abc  |
|+bc     | 1.0   |   0.0 |   0.0 |  100. |  17.  | 130.  |  0.   |  100. |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Property identification number (Integer > 0).

CP         Coordinate system for locating points 1 and 4 (Integer >= 0).

LISTW      The ID of an AEFACT data card which lists (x,y) pairs of
           structural interpolation grid points of the wing (Integer > 0).

LISTC1,LISTC2  The ID of AEFACT data cards which list (x,y) pairs for controls
           (if they exist) (Integers >= 0).

X1,Y1,Z1;X4,Y4,Z4  Location of points 1 and 4 in coordinate system CP (Real).

X12,X43    Edge chord lengths (in aerodynamic coordinate system) (Real >= 0,
           X12 not equal 0.).

## Remarks

1. The x,y pairs of LISTW, LISTC1, and LISTC2 (AEFACT) data cards are in the
   aero element coordinate system.

2. If cranks and/or control surfaces exist, their locations are given on the
   PAERO3 data card.

3. The numbering system and coordinate system are shown in Figure 2.4-1. The
   following twelve points are defined for each Mach Box lifting surface.

   Planform Corners

   1. Leading edge, inboard
   2. Trailing edge, inboard
   3. Trailing edge, outboard
   4. Leading edge, outboard

   Cranks

   5. Leading edge
   6. Trailing edge

   Control

   7. Hinge line, inboard
   8. On inboard edge (usually at trailing edge)
   9. Hinge line, outboard
   10. On outboard edge

   Control (if two)

   9. Hinge line, inboard
   10. On inboard edge (usually at trailing edge)
   11. Hinge line, outboard
   12. On outboard edge (usually at trailing edge)

                              5                  4
        1 *-------------------*----------------*------- y
          |                                    |         elem
          |                                    |
          |      +          +                  |
          |                              + --------- LISTW grid points
          |                                    |
          |   +             +                  |
          |                                    |
          |    7             9         11      |
          |     *------------*--------*        |
          |     | +        + | +   +  |        |
          |    8*         +  *10 +  + *12      |
        2 *-----+---------|--*------|-+--------* 3
          |               |  6      |                    É
          |               |         + LISTC2 grid points ºif control
          |               +---------- LISTC1 grid points ºsurfaces exist
           x     = x                                     È
            aero    elem

             Figure 2.4-1. CAERO3 numbering and coordinate system


# CAERO4 - Aerodynamic Macro-Strip Element Connection

## Description

Defines an aerodynamic macro element for strip theory.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAERO4  | EID   | PID   |  CP   | NSPAN | LSPAN |       |       |       |ABC  |
|+BC     | X1    |  Y1   |   Z1  |  X12  |  X4   |  Y4   |  Z4   |  X43  |     |
|CAERO4  | 6000  | 6001  | 100   |       |  315  |       |       |       |abc  |
|+bc     | 0.0   |  0.0  |   0.0 |  1.0  |  0.2  |  1.0  |  0.0  |  0.8  |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Property identification number (Integer > 0).

CP         Coordinate system for locating points 1 and 4 (Integer >= 0).

NSPAN      Number of strips; if a positive value is given, NSPAN equal strips
           are assumed. If zero or blank, use LSPAN field (Integer >= 0).

LSPAN      ID of an AEFACT data card containing a list of division points for
           strips. Used only if field 5 is zero or blank (Integer > 0 if
           NSPAN is zero or blank).

X1,Y1,Z1;X4,Y4,Z4  Location of points 1 and 4 in coordinate system CP (Real).

X12,X43    Edge chord lengths in aerodynamic coordinate system (Real >= 0,
           and not both zero).


      --+---     1 *-----+-----+------+-------------+--*------+----- y
        |          |     |     |      |             |  | 4    |       elem
        |          |     |     |      |             |  |      |
        |          |     |     |      |             |  |      |
       X           |     |     |      |             |  |     X
        12         |     |     |      |             |  |      43
        |          |     |     +------+-------------+  |      |
        |          |     |     |      |             |  |      |
      --+---     2 *-----+-----+------+-------------+--* 3  --+--
                   |
                   |
                    x     = x
                     aero    elem

## Remarks

1. The strips are numbered sequentially, beginning with EID. You must ensure
   that all strip numbers are unique and different from structural grid ID's.

2. The number of division points is one greater than the number of boxes.
   Thus, if NSPAN = 3, the division points are 0.0, 0.333, 0.667, 1.000. If
   you supply division points, the first and last points need not be 0. and 1.
   (In which case the corners of the panel would not be at the reference
   points.)

3. A triangular element is formed if X12 or X43 = 0.


# CAERO5 - Aerodynamic Macro-Piston Theory Element Connection

## Description

Defines an aerodynamic macro-element for piston theory.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAERO5  | EID   | PID   |  CP   | NSPAN | LSPAN | NTHRY |NTHICK |       |ABC  |
|+BC     | X1    |  Y1   |   Z1  |  X12  |  X4   |  Y4   |  Z4   |  X43  |     |
|CAERO5  | 6000  | 6001  | 100   |       |  315  |   0   |   0   |       |abc  |
|+bc     | 0.0   |  0.0  |   0.0 |  1.0  |  0.2  |  1.0  |  0.0  |  0.8  |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Property identification number (Integer > 0).

CP         Coordinate system for locating points 1 and 4 (Integer >= 0).

NSPAN      Number of strips; if a positive value is given, NSPAN equal strips
           are assumed. If zero or blank, use LSPAN field.

LSPAN      ID of an AEFACT data card containing a list of division points for
           strips. Used only if field 5 is zero or blank (Integer > 0 if
           NSPAN is zero or blank).

NTHRY      Parameter to select the theory (Integer 0, blank, 1, or 2). See
           Remark 4.

           0  Use Piston Theory.
           1  Use Van Dyke Theory (no sweep correction, sec(sweep angle) =
              1.).
           2  Use Van Dyke theory with sweep correction.

NTHICK     Parameter to select thickness integrals input (Integer >= O or
           blank).

           0  Thickness integrals are computed internally.

           >0 Thickness integrals are input directly and is the ID number of
              AEFACT data card which lists the I and/or J integrals.

X1,Y1,Z1;X4,Y4,Z4  Location of points 1 and 4 in coordinate system CP (Real).

X12,X43    Edge chord lengths in aerodynamic coordinate system (Real >= 0,
           and not both zero).


      --+---     1 *-----+-----+------+-------------+--*------+----- y
        |          |     |     |      |             |  | 4    |       elem
        |          |     |     |      |             |  |      |
        |          |     |     |      |             |  |      |
       X           |     |     |      |             |  |     X
        12         |     |     |      |             |  |      43
        |          |     |     +------+-------------+  |      |
        |          |     |     |      |             |  |      |
      --+---     2 *-----+-----+------+-------------+--* 3  --+--
                   |
                   |
                    x     = x
                     aero    elem


## Remarks

1. The strips are numbered sequentially, beginning with EID. You must ensure
   that all strip numbers are unique and different from structural grid ID's.

2. The number of division points is one greater than the number of boxes.
   Thus, if NSPAN = 3, the division points are 0.0, 0.333, 0.667, 1.000. If
   you supply division points, the first and last points need not be 0. and 1.
   (in which the corners of the panel would not be at the reference points).

3. A triangular element is formed if X12 or X43 = 0.

4. Three separate piston theory formulations are available (see Section
   1.11.2.5).

5. I and J thickness integral definitions are shown in Figure 2.4-2. See
   PAERO5 for a method to have these integrals computed internally.

                                   

                          dg
                     g  ð -- = slope of airfoil semithickness
                      î   dî

                    ô 1                                ô 1
              I  =  |   g dî                     J  =  |    g dî
               1    õ0   î                        1    õî    î
                                                         h
                    ô 1                                ô 1
              I  =  |   îg dî                    J  =  |   îg dî
               2    õ0    î                       2    õî    î
                                                         h

                    ô 1                                ô 1
              I  =  |   îýg dî                   J  =  |   îýg dî
               3    õ0     î                      3    õî     î
                                                         h

                    ô 1                                ô 1
              I  =  |   gýdî                     J  =  |    gýdî
               4    õ0   î                        4    õî    î
                                                         h

                    ô 1                                ô 1
              I  =  |   îgýdî                    J  =  |   îgýdî
               5    õ0    î                       5    õî    î
                                                         h

                    ô 1                                ô 1
              I  =  |   îýgýdî                   J  =  |   îýgýdî
               6    õ0     î                      6    õî     î
                                                         h

      Figure 2.4-2. CAERO5 I and J thickness integral definitions


# CAXIFi - Fluid Element Connections

## Description

Defines an axisymmetric fluid element which connects i = 2, i = 3, or i = 4
fluid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAXIF2  |  EID  |  IDF1 |  IDF2 |       |       |  RHO  |   B   |       |     |
|CAXIF2  |   11  |   23  |   25  |       |       |.25E-03|       |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAXIF3  |  EID  |  IDF1 |  IDF2 |  IDF3 |       |  RHO  |   B   |       |     |
|CAXIF3  |  105  |   31  |   32  |   33  |       |       | 6.7E4 |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CAXIF4  |  EID  |  IDF1 |  IDF2 |  IDF3 |  IDF4 |  RHO  |   B   |       |     |
|CAXIF4  |  524  |  421  |  425  |  424  |  422  |  .5-3 | 2.5+3 |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

IDFj       Identification numbers of connected GRIDF points, j = 1,2,...i
           (Integer > 0).

RHO        Fluid density in mass units (Real > 0.0 or blank).

B          Fluid bulk modulus (Real >= 0.0 or blank).

## Remarks

1. This card is allowed only if an AXSLOT card is also present.

2. The element identification number (EID) must be unique with respect to all
   other fluid or structural elements.

3. If RHO or B is blank the corresponding values on the AXSLOT data card are
   used, in which case the default must not be blank (undefined).

4. Plot elements are generated for these elements. Because each plot element
   connects two points, one is generated for the CAXIF2 element, three are
   generated for the CAXIF3 element, and four plot elements are generated for
   the CAXIF4 element. In the last case the elements connect the pairs of
   points (1-2), (2-3), (3-4), and (4-1).

5. If B = 0.0, the fluid is considered to be incompressible.


# CBAR - Simple Beam Element Connection

## Description

Defines a simple beam element (BAR) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CBAR    |  EID  |  PID  |   GA  |  GB   |X1 ,GO |  X2   |   X3  |   F   |abc  |
|+bc     |  PA   |   PB  |  Z1A  |  Z2A  |  Z3A  |  Z1B  |  Z2B  |  Z3B  |     |
|CBAR    |   2   |   39  |   7   |   3   |  13   |       |       |   2   |123  |
|+23     |       |  513  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PBAR property card (Default is EID
           unless BAROR card has nonzero entry in field 3) (Integer > 0 or
           blank). See BAROR card for default options.

GA, GB     Grid point identification numbers of connection points (Integer >
           0; GA not equal GB).

X1, X2, X3 Components of vector v, at end a (Figure 1.3-1a in Section
           1.3.2.1), measured at end a, parallel to the components of the
           displacement coordinate system for GA, to determine (with the
           vector from end a to end b) the orientation of the element
           coordinate system for the bar element (Real, X1**2 + X2**2 + X3**2
           > 0 or blank). See BAROR card for default options.

GO         Grid point identification number to optionally supply X1, X2, X3
           (integer > 0 or blank). See BAROR card for default options.

F          Flag to specify the nature of fields 6-8 as follows:

                           6       7       8
           +----------+-------+---------+---------+
           |F = blank |       |         |         |
           +----------+-------+---------+---------+
           |F = 1     |   X1  |   X2    |   X3    |
           +----------+-------+---------+---------+
           |F = 2     |   GO  | blank/0 | blank/0 |
           +----------+-------+---------+---------+

           This F flag is optional (not required). See BAROR card for default
           options.

PA, PB     Pin flags for bar ends a and b, respectively, that are used to
           insure that the bar cannot resist a force or moment corresponding
           to the pin flag at that respective end of the bar. (Up to 5 of the
           unique digits 1 - 6 anywhere in the field with no imbedded blanks;
           integer > 0) (These degree of freedom codes refer to the element
           forces and not global forces. The bar must have stiffness
           associated with the pin flag. For example, if pin flag 4 is
           specified, the bar must have a value for J, the torsional
           constant.)

Z1A,Z2A,Z3A;Z1B,Z2B,Z3B  Components of offset vectors wa and wb, respectively,
           (see Figure 1.3-1a in Section 1.3.2.1) in displacement coordinate
           systems at points GA and GB, respectively. (Real or blank).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. For an explanation of bar element geometry, see Section 1.3.2.

3. Zero (0) must be used in fields 7 and 8 in order to override entries in
   these fields associated with F = 1 in field 9 on a BAROR card.

4. If there are no pin flags or offsets, the continuation card may be omitted.

5. If bar offset vectors are present, NASTRAN plotting will plot the bar
   connecting to the tip of the offset, not to the associating grid point.


# CCONEAX - Axisymmetric Shell Element Connection

## Description

Defines the connection of a conical shell element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CCONEAX |  EID  |  PID  |  RA   |  RB   |       |       |       |       |     |
|CCONEAX |   1   |   2   |   3   |   4   |       |       |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (1 <= Integer <= 9999).

PID        Identification number of a PCONEAX card (default is EID) (Integer
           > 0).

RA         Identification number of a RINGAX card (Integer > 0; RA not equal
           RB).

RB         Identification number of a RINGAX card (Integer > 0; RA not equal
           RB).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.


# CDAMP1 - Scalar Damper Connection

## Description

Defines a scalar damper element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CDAMP1  |  EID  |  PID  |  G1   |  C1   |  G2   |  C2   |       |       |     |
|CDAMP1  |   19  |   6   |   0   |       |  23   |  2    |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PDAMP property card (default is EID)
           (Integer > 0).

G1, G2     Geometric grid point identification number (Integer >= 0).

C1, C2     Component number (6 >= Integer >= 0).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank or zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CDAMP3 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. The two connection points, (G1, C1) and (G2, C2), must be distinct.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

5. In heat transfer analysis, the CDAMP1 card may be used to define a lumped
   thermal capacitance Q=BT (if connected to grid point S1).


# CDAMP2 - Scalar Damper Property and Connection

## Description

Defines a scalar damper element of the structural model without reference to a
property value.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CDAMP2  |  EID  |   B   |  G1   |  C1   |  G2   |  C2   |       |       |     |
|CDAMP2  |   16  |-2.98  |  32   |  1    |       |       |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

B          The value of the scalar damper (Real).

G1, G2     Geometric grid point identification number (Integer >= 0).

C1, C2     Component number (6 >= Integer >= 0).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank or zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CDAMP4 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This single card completely defines the element since no material or
   geometric properties are required.

4. The two connection points, (G1, C1) and (G2, C2), must be distinct.

5. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

6. In heat transfer analysis the CDAMP2 card may be used to define a lumped
   thermal capacitance Q=BT (if connected to grid point S1).


# CDAMP3 - Scalar Damper Connection

## Description

Defines a scalar damper element of the structural model which is connected
only to scalar points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CDAMP3  |  EID  |  PID  |  S1   |  S2   |  EID  |  PID  |  S1   |  S2   |     |
|CDAMP3  |   16  |  978  |  24   |  36   |   17  |  978  |  24   |  37   |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PDAMP property card (default is EID)
           (Integer > 0).

S1, S2     Scalar point identification numbers (Integer >= 0; S1 not equal
           S2).

## Remarks

1. S1 or S2 may be blank or zero indicating a constrained coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. One or two scalar damper elements may be defined on a single card.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

5. In heat transfer analysis the CDAMP3 card may be used to define a lumped
   thermal capacitance Q=BT (if connected to grid point S1).


# CDAMP4 - Scalar Damper Property and Connection

## Description

Defines a scalar damper element of the structural model which is connected
only to scalar points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CDAMP4  |  EID  |   B   |  S1   |  S2   |  EID  |   B   |  S1   |  S2   |     |
|CDAMP4  |   16  | -2.6  |  4    |  9    |   17  | +8.6  |  3    |  7    |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

B          The scalar damper value (Real).

S1, S2     Scalar point identification numbers (Integer >= 0; S1 not equal
           S2).

## Remarks

1. S1 or S2 may be blank or zero indicating a constrained coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This card completely defines the element since no material or geometric
   properties are required.

4. One or two scalar damper elements may be defined on a single card.

5. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

6. In heat transfer analysis the CDAMP4 card may be used to define a lumped
   thermal capacitance Q=BT (if connected to grid point S1).


# CDUMi - Dummy Element Connection

## Description

Defines a dummy element (1 <= i <= 9).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CDUMi   |  EID  |  PID  |   G1  |  G2   |  G3   |  G4   | etc.  |   GN  |abc  |
| +bc    |  A1   |  A2   | etc.  |       |       |  AN   |       |       |     |
|CDUM2   |  114  |  108  |    2  |   5   |   6   |   8   |       |   11  |ABC  |
| +BC    |  2.4  |       | 3.E4  |   2   |       |  50   |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of a PDUMi property card (Integer > 0).

G1...GN    Grid point identification numbers of connection points (Integer >
           0, G1 through GN must be unique).

A1...AN    Additional entries (Real or Integer).

## Remarks

1. You must code the associated element routines for matrix generation, stress
   recovery, etc., and perform a link edit to replace the dummy routines.

2. If no property card is required, field 3 may contain the material
   identification number.

3. Additional entries are defined in your element routines.


# CELAS1 - Scalar Spring Connection

## Description

Defines a scalar spring element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CELAS1  |  EID  |  PID  |   G1  |  C1   |  G2   |  C2   |       |       |     |
|CELAS1  |   2   |   6   |       |       |   8   |   1   |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PELAS property card (default is EID)
           (Integer > 0).

G1, G2     Geometric grid point identification number (Integer > 0).

C1, C2     Component number (6 >= Integer >= 0).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank or zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CELAS3 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. The two connection points, (G1, C1) and (G2, C2), must be distinct.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

5. In heat transfer analysis the CELAS1 card may be used to define a
   conduction or convection between two points or to ground (Q = K*dT) where
   dT is delta T.


# CELAS2 - Scalar Spring Property and Connection

## Description

Defines a scalar spring element of the structural model without reference to a
property value.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CELAS2  |  EID  |   K   |   G1  |  C1   |  G2   |  C2   |  GE   |   S   |     |
|CELAS2  |   28  | 6.2+3 |   32  |       |  19   |   4   |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (0 < Integer <= 10**7 if
           acoustic).

K          The value of the scalar spring (Real).

G1, G2     Geometric grid point identification number (Integer >= 0).

C1, C2     Components number (6 >= Integer >= 0).

GE         Damping coefficient (Real).

S          Stress coefficient (Real).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank of zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CELAS4 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This single card completely defines the element since no material or
   geometric properties are required.

4. The two connection points, (G1, C1) and (G2, C2), must be distinct.

5. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

6. In heat transfer analysis the CELAS2 card may be used to define a
   conduction or convection between two points or to ground (Q = K*dT) where
   dT is delta T.


# CELAS3 - Scalar Spring Connection

## Description

Defines a scalar spring element of the structural model which is connected
only to scalar points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CELAS3  |  EID  |  PID  |  S1   |  S2   |  EID  |  PID  |  S1   |  S2   |     |
|CELAS3  |   19  |   2   |  14   |  15   |   2   |   3   |  0    |  28   |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PELAS property card (default is EID)
           (Integer > 0).

S1, S2     Scalar point identification numbers (Integer >= 0; S1 not equal
           S2).

## Remarks

1. S1 or S2 may be blank or zero indicating a constrained coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. One or two scalar springs may be defined on a single card.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

5. In heat transfer analysis the CELAS3 card may be used to define a
   conduction or convection between two points or to ground (Q=K*dT) where dT
   is delta T.


# CELAS4 - Scalar Spring Property and Connection

## Description

Defines a scalar element of the structural model which is connected only to
scalar points without reference to a property value.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CELAS4  |  EID  |   K   |  S1   |  S2   |  EID  |   K   |  S1   |  S2   |     |
|CELAS4  |   42  | 6.2-3 |   2   |       |   13  | 6.2-3 |  0    |   2   |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

K          The scalar spring value (Real).

S1, S2     Scalar point identification numbers (Integer >= 0; S1 not equal
           S2).

## Remarks

1. S1 or S2, but not both, may be blank or zero indicating a constrained
   coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This card completely defines the element since no material or geometric
   properties are required.

4. No damping coefficient is available with this form. (Assumed to be 0.0.)

5. No stress coefficient is available with this form.

6. One or two scalar springs may be defined on a single card.

7. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

8. In heat transfer analysis the CELAS4 card may be used to define a
   conduction or convection between two points or to ground (Q=K*dT) where dT
   is delta T.

# CELBOW - Curved Beam or Elbow Element

## Description

Defines a curved beam or elbow element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CELBOW  |  EID  |  PID  |  GA   |  GB   |   X1  |   X2  |   X3  |   1   |     |
|CELBOW  |   29  |   2   |   3   |  45   |  -1.0 |   0.0 |  0.0  |   1   |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of a PELBOW property card (Integer > 0).

GA, GB     Grid point identification numbers of connection points (Integer >
           O; GA not equal GB).

X1, X2, X3 Components of vector v at end A (see Figure 2.4-3 below), measured
           at end A, parallel to the components of the displacement
           coordinates for GA. Vector points the direction from GA to C
           (center of curvature), and is used to orient the element
           coordinate system for the ELBOW (real, X1**2 + X2**2 + X3**2 > 0).

## Remarks

1. The product moment of inertia is neglected (I12 = 0). This assumes that at
   least one axis of symmetry of the element cross section exists, for
   example, tube, I-beam, channel, tee, etc.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. There are no pin flags or offsets permitted with the ELBOW element.

4. The local element coordinate system is shown in Figure 2.4-3. Plane 1
   contains the points GA and GB and the vector v. Plane 2 is normal to Plane
   1 and contains the vector v.

5. Element forces and stresses are oriented in the element coordinate system
   at the A-end, and in a rotated coordinate system at the B-end which is
   tangent to the curved beam at the B-end.

6. Field 9 must always have an integer value of 1.

             +M
    T        | 2b
     b       |     V
     +----+--- +    2
        F    | |M
 GB      xb  | | 1b
             + |
           V   |
            1b |
               |                                    + X
               |                                    |  e
               |         Y         ->        M      |
               |          e         v         1a    | V      GA
               +----------+---------+---------------+  2
           C   Center of                  M  +----- | -------+V
               Curvature                   2a       +F         1a
                                                    | xa
                                                    |
                                                    +T
                                                      a

             Figure 2.4-3. CELBOW element local coordinate system


# CEMLOOP - Circular Current Loop

## Description

Defines a circular current loop in magnetic field problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CEMLOOP |  SID  |   I   |  AXI  |  X1   |  Y1   |  Z1   |  X2   |   Y2  |+a   |
|+a      |  Z2   |  XC   |  YC   |  ZC   |  CID  |       |       |       |     |
|CEMLOOP |   3   |  2.5  |   1   |  5.2  |  0.0  | 2.25  |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

I          Current through loop (units of positive charge/sec) (Real > 0.0).

AXI        = 0, nonaxisymmetric problem.
           = 1, axisymmetric problem; TRAPRG and TRIARG elements are implied
           (Integer).

X1,Y1,Z1;X2,Y2,Z2  Coordinates of two points through which the loop passes 
           (given in coordinate system CID) (Real).

XC, YC, ZC Coordinates of center of loop (given in coordinate system CID)
           (Real).

CID        Coordinate system identification number (Integer > 0).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. If AXI = 1, Y1 must be 0.0 or blank, and all data fields after Z1 must be
   0.0 or blank. (Continuation card need not be present.)

3. CID must be 0 or blank.

4. Points 1 and 2, (X1, Y1, Z1) and (X2, Y2, Z2), must be distinct and must be
   equidistant from the center of the circle. Also, the center of the circle
   and the two points must be non-collinear.

5. The direction of current is assumed to be from point 1 to point 2.

6. These computations involve elliptic integrals computed by an iterative
   process with a default convergence criterion of 1.E-6. The criterion can be
   changed with a PARAM bulk data card. At most 15,000 iterations are
   performed. With these parameters, convergence will occur when an
   integration or grid point is no closer to the loop than an amount equal to
   2% of the radius. A convergence criterion of 1.E-5 will allow the point to
   be much closer to the loop. If convergence fails, a message is output, and
   the computations continue with the last iterated value. 


# CFFREE - Free Fluid Surface
## Description

Defines the fluid elements composing the free fluid surface in a hydroelastic
analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFFREE  | EIDF  | GRAVID| FACE  |       | EIDF  | GRAVID| FACE  |       |     |
|CFFREE  |  100  |  100  |  3    |       |  101  |  100  |  4    |       |     |

Field      Contents
-----      --------
EIDF       Fluid element identification number (Integer > 0) (see Remark 1).

GRAVID     Identification number of a GRAV gravity vector set (Integer > 0).

FACE       Identification number of the face of the fluid element, EIDF,
           forming the free surface (0 < Integer <= 6) (see Remark 2).

## Remarks

1. Allowable fluid element types are CFHEX1, CFHEX2, CFTETRA, and CFWEDGE.

2. The numbering conventions for solid faces are defined in fluid element
   connection bulk data card descriptions.


# CFHEXi - Fluid Hexahedral Element Connection

## Description

Defines two types of fluid hexahedral elements (three-dimensional solids with
eight vertices and six quadrilateral faces) to be used in hydroelastic
analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFHEXi  |  EID  |  MID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |   G7  |   G8  |       |       |       |       |       |       |     |
|CFHEX2  |   15  |  100  |   1   |   2   |   3   |   4   |   5   |   6   |ABC  |
|+BC     |    7  |    8  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
CFHEXi     CFHEX1 or CFHEX2 (BCD) (see Remark 4).

EID        Element identification number (Integer > 0).

MID        Material identification number (Integer > 0).

G1,...,G8  Grid point identification numbers of connection points (Integers >
           0, G1 through G8 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. The numbering and order of the grid points and faces, required for
   specifying free fluid surfaces, are defined in Figure 2.4-4.

3. The quadrilateral faces must be nearly planar.

4. CFHEX1 is developed by 5 tetrahedra, CFHEX2 by 10 overlapping tetrahedra.

5. Material identification number must reference a MATF bulk data card.

                                            F6 (TOP)
                        G8                  /          G7
                         +-----------------------------+
                        /                 /           /|
                       / |               /           / |
                      /            F4               /  |
                     /   |                      G6 /   |
                  G5+-----------------------------+ F3 |
                    | F5 |_ _ _ _ _ _ _ _ _ _ _ _ |_ _ |
                    |    /G4                      |    /G3
                    |   /                         |   /
                    |  /          F2              |  /
                    | /                   /       | /
                    |/                   /        |/
                    +-----------------------------+
                   G1                  /         G2
                                      F1 (BOTTOM)

                        Note: Fn indicates a face number

            Figure 2.4-4. CFHEXi grid point identification numbers
                                   


# CFLSTR - Fluid/Structure Interface

## Description

Defines fluid/structure interfaces in hydroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFLSTR  | EIDF  |GRAVID | EIDS1 | EIDS2 | EIDS3 | EIDS4 | EIDS5 | EIDS6 |abc  |
|+bc     | EIDS7 | EIDS8 |       |  etc. |       |       |       |       |def  |
|CFLSTR  |  100  |  10   |   1   |   2   |  11   |  12   |  21   |  22   |ABC  |
|+BC     |   31  |   32  |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFLSTR  | EIDF  |GRAVID | EID1  | "THRU"| EID2  |       |       |       |     |
|CFLSTR  |  200  |  100  |  101  |  THRU |  106  |       |       |       |     |

Field      Contents
-----      --------
EIDF       Fluid element identification number (Integer > 0) (see Remark 3).

GRAVID     Identification number of a GRAV gravity vector set (Integer > 0).

EIDSi, EIDiStructural element identification numbers (Integer > 0).

## Remarks

1. As many continuation cards as desired may appear when THRU is not used.

2. All element identification numbers between EID1 and EID2 must exist when
   using the THRU option.

3. Allowable fluid element types are CFHEX1, CFHEX2, CFTETRA, and CFWEDGE.


# CFLUIDi - Fluid Element Connections

## Description

Defines three types of fluid elements for axisymmetric fluid model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFLUID2 |  EID  |  IDF1 |  IDF2 |       |       |  RHO  |   B   |       |     |
|CFLUID2 |  100  |   11  |   14  |       |       | .025  |  0.0  |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFLUID3 |  EID  |  IDF1 |  IDF2 |  IDF3 |       |  RHO  |   B   |       |     |
|CFLUID3 |  110  |   15  |   13  |   12  |       |  1.2  |       |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFLUID4 |  EID  |  IDF1 |  IDF2 |  IDF3 |  IDF4 |  RHO  |   B   |       |     |
|CFLUID4 |  120  |   11  |   15  |   12  |   14  |       |       |       |     |

Field      Contents
-----      --------
EID        Element Identification number (Integer, 0 < Idc <  10**5).

IDFi       Identification number of RINGFL card (Integer > 0; IDF1 through
           IDF4 must be unique).

RHO        Mass density (Real >  0.0 or blank; if blank, the AXIF default
           value is used).

B          Bulk modulus, pressure per volume ratio (Real or blank. Default
           value on AXIF card is used if blank.)

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. Element identification number must be unique with respect to all other
   fluid, scalar, and structural elements.

3. The volume defined by IDFi is a body of revolution about the polar axis of
   the fluid coordinate system defined by AXIF. CFLUID2 defines a thick disk
   with IDF1 and IDF2 defining the outer corners as in Figure 2.4-5.

4. All interior angles must be less than 180 degrees.

5. The order of connected RINGFL points is arbitrary.

6. If the bulk modulus value is zero the fluid is assumed incompressible.

                                        15
                                         *
                                        /|\
                                       / | \
                       +              /  |  \
               Polar   |         14  /   |   \
               axis    *____________/120 |    \     í = 0.0
                       |            \   F|     \
                       |             \   |      \
                       |  100         \  | 110   \
                       |     F         \ |    F   \
                       *-----------------*---------*
                       |              11 12       13
                       +-------------------------------------- Radius

                       Figure 2.4-5. CFLUID2 coordinates


# CFTETRA - Fluid Tetrahedral Element Connection

## Description

Defines a fluid tetrahedral element (three-dimensional solid, with four
vertices and four triangular faces) to be used in hydroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFTETRA |  EID  |  MID  |  G1   |  G2   |  G3   |  G4   |       |       |     |
|CFTETRA |   25  |   6   |   1   |   2   |   3   |   4   |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

MID        Material identification number (Integer > 0).

G1,G2,G3,G4Grid point identification numbers of connection points (Integers >
           0, G1 through G4 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. The numbering of the grid points and faces, required for specifying free
   fluid surfaces, is defined in Figure 2.4-6.

3. Material identification number must reference a MATF bulk data card.

                                         G4
                                         *
                                        /|\
                                       / | \  F4
                                      /  |  \/
                                     /   |  /\
                                  G1*  F2|    \
                                    \ .  |     \
                                     \  .|  F3  \
                                      \  | .     \
                                       \ |    .   \
                                         *---------*
                                        G2 |       G3
                                           |
                                           F1

                        Note: Fn indicates a face number

            Figure 2.4-6. CFTETRA grid point identification numbers


# CFTUBE - Fluid Tube Connection

## Description

Defines a fluid tube element of the heat transfer model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFTUBE  |  EID  |  PID  |  G1   |  G2   |       |       |       |       |     |
|CFTUBE  |  200  |   5   |   8   |  12   |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of PFTUBE property card (Integer > 0).

G1, G2     Identification numbers of connected grid points (Integers > 0).

## Remarks

1. The FTUBE element should only be used in nonlinear and transient heat
   transfer analysis. Use in linear static analysis produces an unsymmetric
   matrix which leads to incorrect results.

2. The positive direction for flow is from G1 to G2.

3. It is your responsibility to ensure flow continuity. There must be no
   accumulation of fluid mass at any grid point.


# CFWEDGE - Fluid Wedge Element Connection

## Description

Defines a fluid wedge element (three-dimensional solid, with three
quadrilateral faces and two opposing triangular faces) to be used in
hydroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CFWEDGE |  EID  |  MID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |     |
|CFWEDGE |   25  |  100  |   1   |   2   |   3   |   4   |   5   |   6   |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

MID        Material identification number (Integer > 0).

G1,...,G6  Grid point identification numbers of connection points (Integers >
           0; G1 through G6 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. The numbering of the grid points and faces, required for specifying free
   fluid surfaces, is defined in Figure 2.4-7.

3. The quadrilateral faces must be nearly planar.

4. Material identification number must reference a MATF bulk data card.

                        G4  __________ G6
                           |\        /|
                           | \  F5  /-|--- F4
                           |  \    /  |
                           |F2 \G5/F3 |
                        G1 |- - \/- - | G3     Note: Fn indicates a face number
                            \    |    /
                             \   |   /
                              \  | -/---- F1
                               \ | /
                                \|/
                                 *
                                 G2

            Figure 2.4-7. CFWEDGE grid point identification numbers


# CHBDY - Heat Boundary Element

## Description

Defines a boundary element for heat transfer analysis which is used for heat
flux, thermal vector flux, convection, and/or radiation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CHBDY   |  EID  |  PID  |  TYPE |  G1   |  G2   |  G3   |  G4   |       |abc  |
|+bc     |  GA1  |  GA2  |  GA3  |  GA4  |  V1   |  V2   |  V3   |       |     |
|CHBDY   |  721  |  100  |  LINE |  101  |  98   |       |       |       |+BD21|
|+BD21   |  102  |  102  |       |       | 1.00  |  0.0  |  0.0  |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Property identification number (Integer > 0).

TYPE       Type of area involved (must be one of POINT, LINE, REV, AREA3,
           AREA4, or ELCYL).

G1,...,G4  Grid point identification numbers of primary connected points
           (Integer > 0 or blank).

GA1,...,GA4Grid or scalar point identification numbers of associated ambient
           points (Integer > 0 or blank).

V1, V2, V3 Vector (in the basic coordinate system) used for element
           orientation (real or blank).

## Remarks

1. The continuation card is not required.

2. The six types have the following characteristics:

   a. The POINT type has one primary grid point and requires a property card,
      and the normal vector (V1, V2, V3) must be given if thermal vector flux
      is to be used.

   b. The LINE type has two primary grid points and requires a property card,
      and the vector is required if thermal vector flux is to be used.

   c. The REV type has two primary grid points which must lie in the x-z plane
      of the basic coordinate system with x > 0. The defined area is a conical
      section with z as the axis of symmetry. A property card is required for
      convection, radiation, or thermal vector flux.

   d. The AREA3 and AREA4 types have three and four primary grid points,
      respectively. These points define a triangular or quadrilateral surface
      and must be ordered to go around the boundary. A property card is
      required for convection, radiation, or thermal vector flux.

   e. The ELCYL type (elliptic cylinder) has two connected primary grid points
      and requires a property card, and if thermal vector flux is used, the
      vector must be nonzero.

3. A property card, PHBDY, is used to define the associated area factors, the
   emissivity, the absorbivity, and the principal radii of the elliptic
   cylinder. The material coefficients used for convection and thermal
   capacity are referenced by the PHBDY card. See this card description for
   details.

4. The associated points, GA1, GA2, etc., may be either grid or scalar points,
   and are used to define the fluid ambient temperature when a convection
   field exists. These points correspond to the primary (CHBDY element) points
   G1, G2, etc., and the number of them depends on the TYPE option, but they
   need not be unique. Their values may be set in statics with an SPC card, or
   they may be connected to other elements. If any field is blank, the ambient
   temperature associated with that grid point is assumed to be zero.

5. Heat flux may be applied to this element with QBDY1 or QBDY2 cards.

6. Thermal vector flux from a directional source may be applied to this
   element with a QVECT card. The orientation of the normal vector must be
   defined. The grid point ordering establishes the normal vector direction as
   end a to end b for line elements and right hand rule for cross product
   elements. See Section 1.8 for the definition of the normal vector for each
   element type.


# CHEXAi - Hexahedron Element Connection

## Description

Defines two types of hexahedron elements (3 dimensional solid with 8 vertices
and 6 quadrilateral faces, HEXAi) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CHEXAi  |  EID  |  MID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |       |       |       |       |       |       |     |
|CHEXA2  |   15  |   2   |   7   |   8   |   9   |  10   |  15   |  16   |ABC  |
|+BC     |  17   |  18   |       |       |       |       |       |       |     |

Field      Contents
-----      --------
CHEXAi     CHEXA1 or CHEXA2 (see Remark 7).

EID        Element identification number (Integer > 0).

MID        Material identification number (Integer > 0).

G1,...,G8  Grid point identification numbers of connection points (Integers >
           0, G1 through G8 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. The order at the grid points is: G1, G2, G3, G4 in order around one
   quadrilateral face. G5, G6, G7, G8 are in order in the same direction
   around the opposite quadrilateral, with G1 and G5 along the same edge.

3. The quadrilateral faces must be nearly planar.

4. There is no nonstructural mass.

5. For structural problems, material must be defined by MAT1 card.

6. Stresses are given in the basic coordinate system.

7. CHEXA1 represents the element as 5 tetrahedra; CHEXA2 represents the
   element as 10 overlapping tetrahedra.

8. For heat transfer problems, material may be defined with either a MAT4 or
   MAT5 card.

                         G8                            G7
                         *-----------------------------*
                        /|                            /|
                       /                             / |
                      /  |                          /  |
                     /                          G6 /   |
                  G5*-----------------------------*    |
                    |    *G4_ _ _ _ _ _ _ _ _ _ _ |_ _ *G3
                    |    /                        |    /
                    |                             |   /
                    |  /                          |  /
                    |                             | /
                    |/                            |/
                  G1*-----------------------------*G2

            Figure 2.4-8. CHEXAi grid point identification numbers


# CIHEX1 - Linear Isoparametric Hexahedron Element Connection

## Description

Defines a linear isoparametric hexahedron element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CIHEX1  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |       |       |       |       |       |       |     |
|CIHEX1  |  137  |   5   |   3   |   8   |   5   |   4   |   9   |  14   |ABC  |
|+BC     |  11   |  10   |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PIP        Identification number of a PIHEX property card (Integer > 0).

G1,...,G8  Grid point identification numbers of connection points (Integers >
           0, G1 through G8 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. Grid points G1, G2, G3, G4 must be given in counter-clockwise order about
   one quadrilateral face when viewed from inside the element. G5, G6, G7, G8
   are in order in the same direction around the opposite quadrilateral, with
   G1 and G5 along the same edge.

3. There is no non-structural mass.

4. The quadrilateral faces need not be planar.

5. Stresses are given in the basic coordinate system.

                         G7                            G6
                         *-----------------------------*
                        /|                            /|
                       /                             / |
                      /  |                          /  |
                     /                          G5 /   |
                  G8*-----------------------------*    |
                    |    *G3_ _ _ _ _ _ _ _ _ _ _ |_ _ *G2
                    |    /                        |    /
                    |                             |   /
                    |  /                          |  /
                    |                             | /
                    |/                            |/
                  G4*-----------------------------*G1

            Figure 2.4-9. CIHEX1 grid point identification numbers


# CIHEX2 - Quadratic Isoparametric Hexahedron Element Connection

## Description

Defines a quadratic isoparametric hexahedron element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CIHEX2  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |  G9   | G10   | G11   | G12   | G13   | G14   |def  |
|+ef     | G15   | G16   | G17   | G18   | G19   | G20   |       |       |     |
|CIHEX2  |  110  |   7   |   3   |   8   |  12   |  13   |  14   |   9   |ABC  |
|+BC     |   5   |   4   |  16   |  19   |  20   |  17   |  23   |  27   |DEF  |
|+EF     |  31   |  32   |  33   |  28   |  25   |  24   |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of a PIHEX property card (Integer > 0).

G1,...,G20 Grid point identification numbers of connection points (Integers >
           0, G1 through G20 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. Grid points G1,...,G8 must be given in counter-clockwise order about one
   quadrilateral face when viewed from inside the element. G9,...,G12 and
   G13,...,G20 are in the same direction with G1, G9 and G13 along the same
   edge.

3. There is no non-structural mass.

4. The quadrilateral faces need not be planar.

5. Stresses are given in the basic coordinate system.

                         G17           G16             G15
                         *-------------*---------------*
                        /                             /|
                       / |                        G14* |
                   G18*  *G11                       /  |
                     /   |         G20         G13 /   *G10
                 G19*--------------*--------------*    |
                    |    *G5_ _ _ _ _ _*_ _ _ _ _ |_ _ *G3
                    |    /             G4         |    /
                 G12*   *G6                       *G9 /
                    |  /                          |  *G2
                    | /                           | /
                    |/                            |/
                    *-------------*---------------*
                    G7            G8              G1

            Figure 2.4-10. CIHEX2 grid point identification numbers


# CIHEX3 - Cubic Isoparametric Hexahedron Element Connection

## Description

Defines a cubic isoparametric hexahedron element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CIHEX3  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |  G9   | G10   | G11   | G12   | G13   | G14   |def  |
|+ef     | G15   | G16   | G17   | G18   | G19   | G20   | G21   | G22   |ghi  |
|+hi     | G23   | G24   | G25   | G26   | G27   | G28   | G29   | G30   |jkl  |
|+kl     | G31   | G32   |       |       |       |       |       |       |     |
|CIHEX3  |   15  |   3   |   4   |   9   |  12   |  17   |  18   |  19   |ABC  |
|+BC     |  20   |  13   |  10   |   7   |   6   |   5   |  22   |  25   |DEF  |
|+EF     |  26   |  23   |  28   |  31   |  32   |  29   |  36   |  41   |GHI  |
|+HI     |  44   |  49   |  50   |  51   |  52   |  45   |  42   |  39   |JKL  |
|+KL     |  38   |  37   |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of a PIHEX property card (Integer > 0).

G1,...,G32 Grid point identification numbers of connection points (Integers >
           0, G1 through G32 must be unique).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. Grid points G1,...,G12 must be given in counter-clockwise order about one
   quadrilateral face when viewed from inside the element. G13,...,G16;
   G17,...,G20; and G21,...,G32 are in the same direction, with G1, G13, G17,
   G21 along the same edge.

3. There is no nonstructural mass.

4. The quadrilateral faces need not be planar.

5. Stresses are given in the basic coordinate system.

                         27       26        25         24
                         *--------*---------*----------*
                      28*|                            /|
                       / *19                       23* *18
                    29*  |                          /  |
                     /   *15 31      32          22*   *14
                  30*--------*-------*------------*21  |
                    |    *7 _ _ *6_ _ _ _*65_ _ _ |_ _ *64
                  20*    /                      17*    /
                    |   *8                        |   *63
                  16*  /                        13*  /
                    | *7                          | *62
                    |/                            |/
                    *--------*---------*----------*61
                    10       11        12

            Figure 2.4-11. CIHEX3 grid point identification numbers


# CIS2D8 - Quadratic Isoparametric Element Connection

## Description

Defines a quadriparabolic isoparametric membrane element (IS2D8) of the
structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CIS2D8  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |+abc |
|+abc    |  G7   |  G8   | ID1   |  TH   |       |       |       |       |     |
|CIS2D8  |   16  |   2   |  12   |  10   |  15   |  18   |  22   |   3   |+ABC |
|+ABC    |   7   |  11   |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

PID        Identification number of a PIS2D8 property card (Integer > 0).

G1,...,G8  Grid point identification numbers of connection points (Integers >
           0; G1 through G8 must be unique).

ID1        Number of Gauss quadrature points (ID1 = 2 or 3; default is 2).

TH         Material property orientation angle in degrees (Real). Figure
           2.4-12 gives the sign convention for TH.

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. Grid points G1 through G8 must be ordered as shown above.

3. This element is a planar element; that is, G1 through G8 must be in a
   plane.

4. Stresses are computed in the element coordinate system.

5. The element may be collapsed to a triangle by having coincident grid points
   or by making two edges collinear (which is the preferred method). If grid
   points are made coincident, the only choices are G2, G6 and G3 or G3, G4
   and G7. Grid points G1, G5, and G2 may not be coincident, nor may grid
   points G1, G8, and G4.

6. The midpoints G5, G6, G7, and G8 should be placed as close to the mid-side
   as possible, except for unusual circumstances, for example, when the
   element is to be used as a crack element.

                                ye
                                |       G7
                             G4 *-------*-------* G3
                                |               |
                                |               |
                                |           .   |
                             G8 *        .      * G6
                                |     .         |
                                |  .            |
                                |   TH  G5      |
                             G1 *-------*-------*------xe
                                                  G2

                 Figure 2.4-12. CIS2D8 sign convention for TH


# CMASS1 - Scalar Mass Connection

## Description

Defines a scalar mass element of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CMASS1  |  EID  |  PID  |  G1   |  C1   |  G2   |  C2   |       |       |     |
|CMASS1  |   32  |   6   |   2   |   1   |   2   |   3   |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PMASS property card (default is EID)
           (Integer > 0).

G1, G2     Geometric grid point identification number (Integer > 0).

C1, C2     Component number (6 >= Integer >= 0).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank or zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CMASS3 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. The two connection points, (G1, C1) and (G2, C2), must be distinct.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.


# CMASS2 - Scalar Mass Property and Connection

## Description

Defines a scalar mass element of the structural model without reference to a
property value.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CMASS2  |  EID  |   M   |  G1   |  C1   |  G2   |  C2   |       |       |     |
|CMASS2  |   32  |  9.25 |   6   |   1   |   7   |       |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

M          The value of the scalar mass (Real).

G1, G2     Geometric grid point identification number (Integer >= 0).

C1, C2     Component number (6 >= Integer >= 0).

## Remarks

1. Scalar points may be used for G1 and/or G2, in which case the corresponding
   C1 and/or C2 must be zero or blank. Zero or blank may be used to indicate a
   grounded terminal G1 or G2 with a corresponding blank or zero C1 or C2. If
   only scalar points and/or ground are involved, it is more efficient to use
   the CMASS4 card. (A grounded terminal is a scalar point or coordinate of a
   geometric grid point whose displacement is constrained to zero.)

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This card completely defines the element since no material or geometric
   properties are required.

4. The two connection points, (G1, C1) and (G2, C2), must be distinct.

5. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.


# CMASS3 - Scalar Mass Connection

## Description

Defines a scalar mass element of the structural model which is connected only
to scalar points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CMASS3  |  EID  |  PID  |  S1   |  S2   |  EID  |  PID  |  S1   |  S2   |     |
|CMASS3  |   13  |   42  |  62   |   1   |       |       |       |       |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

PID        Identification number of a PMASS property card (default is EID)
           (Integer > 0).

S1, S2     Scalar point identification numbers (Integer >= 0; S1 not equal
           S2).

## Remarks

1. S1 or S2 may be blank or zero indicating a constrained coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. One or two scalar masses may be defined on a single card.

4. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.


# CMASS4 - Scalar Mass Property and Connection

## Description

Defines a scalar mass element of the structural model which is connected only
to scalar points without reference to a property value.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CMASS4  |  EID  |   M   |  S1   |  S2   |  EID  |   M   |  S1   |  S2   |     |
|CMASS4  |   23  | 14.92 |   6   |  23   |   2   | -16.3 |   0   |  29   |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

M          The scalar mass value (Real).

S1, S2     Scalar point identification numbers (Integer > 0; S1 not equal
           S2).

## Remarks

1. S1 or S2 may be blank or zero indicating a constrained coordinate.

2. Each element identification number must be unique with respect to all other
   element identification numbers.

3. This card completely defines the element since no material or geometric
   properties are required.

4. One or two scalar masses may be defined on a single card.

5. For a discussion of the scalar elements, see Section 5.6 of the Theoretical
   Manual.

# CNGRNT - Identical (Congruent) Elements Indicator

## Description

Designates secondary element(s) identical (or congruent) to a primary element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CNGRNT  |  PRID | SECID1| SECID2| SECID3| SECID4| SECID5| SECID6| SECID7|abc  |
|CNGRNT  |  11   |   2   |   17  |   34  |   35  |   36  |       |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+bc     | SECID8| SECID9|       |  etc. |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CNGRNT  |  PRID | SECID1| "THRU"| SECID2|       |       |       |       |     |
|CNGRNT  |   7   |   10  |  THRU |   55  |       |       |       |       |     |

Field      Contents
-----      --------
PRID       Identification number of the primary element (not necessarily the
           lowest number).

SECIDi     Identification number(s) of secondary element(s) whose matrices
           will be identical (or congruent) to those of the primary element.

## Remarks

1. Orientation, geometry, etc. must be truly identical such that the same
   stiffness, mass, and damping matrices are generated in the global
   coordinate system.

2. This feature is automatically used by the INPUT module.

3. The CNGRNT feature cannot be used when an AXIC card is present in the Bulk
   Data Deck.

4. An element that has been listed as a primary ID on a CNGRNT card cannot be
   listed as a secondary ID on another CNGRNT card. However, if the element is
   listed as a secondary ID on the same card, then such secondary IDs are
   ignored.

5. The same secondary IDs cannot be listed as congruent to two or more
   different primary IDs.

6. Redundant specifications on CNGRNT cards are ignored.

7. The element IDs (primary or secondary) specified on a CNGRNT card need not
   all exist in a model. This greatly facilitates the use of the THRU option
   on the card. However, you should be cautioned that, if too many
   non-existent elements are specified in the CNGRNT data (as may be the case
   when the THRU option is used), the EMG (Element Matrix Generator) module
   may not have enough core to process all the CNGRNT data. In that case, an
   appropriate message is issued and those elements whose CNGRNT data cannot
   be processed will have their element matrices computed separately.

8. The stiffness, mass, and damping matrices are actually calculated for the
   lowest numbered element in the congruent set (even through this element may
   not be the primary ID).

9. See Section 1.14 for a detailed discussion of the congruent feature.


# CONCT - Substructure Connectivity

## Description

Defines the grid point and degree of freedom connectivities between two
substructures for a manual COMBINE operation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CONCT   |  SID  |   C   |  SUBA |  SUBB |       |       |       |       |def  |
|CONCT   |  307  |  1236 |WINGRT |FUSELAG|       |       |       |       |DEF  |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+ef     |  GA1  |  GB1  |  GA2  |  GB2  |  GA3  |  GB3  |  GA4  |  GB4  |hij  |
|+EF     |  201  |  207  |  958  |  214  |  971  |  216  |  982  |       |HIJ  |

Field      Contents
-----      --------
SID        Identification number of connectivity set (Integer > 0).

C          Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

SUBA, SUBB Names of basic substructures being connected (BCD).

GAi, GBi   Grid or scalar point identification numbers GAi from SUBA connects
           to GBi from SUBB by the degrees of freedom specified in C (Integer
           > 0).

## Remarks

1. At least one continuation card must be present.

2. Components specified on a CONCT card will be overridden by RELES cards.

3. Several CONCT and CONCT1 cards may be input with the same value of SID.

4. An alternate format is given by the CONCT1 data card.

5. Connectivity sets must be selected in the Substructure Control Deck
   (CONNECT = SID)to be used by NASTRAN. Note that CONNECT is a subcommand of
   the substructure COMBINE command.

6. SUBA and SUBB must be component basic substructures of the pseudostructures
   being combined as specified on the substructure COMBINE command card. SUBA
   and SUBB must not be components of the same pseudostructure.

7. If GTRAN has been invoked under the COMBINE command, the entries on the
   CONCT and CONCT1 cards must be defined in terms of the revised coordinate
   system.

   In the following diagram, a substructure tree and a set of substructure
   command cards are shown. The CONNECT subcommand references the example
   CONCT card above. In this example, pseudostructure PSUB1 and PSUB2 are
   combined and connected only at points in their respective basic component
   substructures WINGRT and FUSELAG.

      Basic          +--------+  +-------+      +-------+   +-------+
      Substructures  | WINGRT |  |  SUBC |      |FUSELAG|   |  SUBD |
                     +----+---+  +---+---+      +---+---+   +---+---+
                          +-----+----+              +-----+-----+
                            +---+---+                 +---+---+
   Pseudostructures         | PSUB1 |                 | PSUB2 |
                            +-------+                 +-------+

                          COMBINE(MANUAL) PSUB1,PSUB2

                                NAME = PPSUB
                                TOLER = 0.01
                                CONNECT = 307


# CONCT1 - Substructure Connectivity

## Description

Defines the grid point and degree of freedom connectivities between two or
more substructures for a manual COMBINE operation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CONCT1  |  SID  | NAME1 | NAME2 | NAME3 | NAME4 | NAME5 | NAME6 | NAME7 |def  |
|CONCT1  |  805  |WINGRT |FUSELAG| MIDWG |  POD  |       |       |       |DEF  |
|+ef     |   C1  |  G11  |  G12  |  G13  |  G14  |  G15  |  G16  |  G17  |hij  |
|+ij     |   C2  |  G21  |  G22  |  G23  |  G24  |  G25  |  G26  |  G27  |     |
|+EF     |  123  |  528  |   17  |   32  |  106  |       |       |       |HIJ  |
|+IJ     |   46  |  518  |       |       |       |       |       |       |etc. |

Field      Contents
-----      --------
SID        Identification number of connectivity set (Integer > 0).

NAMEi      Basic substructure name (BCD).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

Gij        Grid or scalar point identification number in substructure namej
           with components Ci (Integer > 0).

## Remarks

1. At least one continuation card must be present.

2. Components specified on CONCT1 card will not be overridden by RELES cards.

3. Several CONCT and CONCT1 cards may be input with the same value of SID.

4. An alternate format is given by the CONCT card.

5. Connectivity sets must be selected in the Substructure Control Deck
   (CONNECT = SID) to be used by NASTRAN. Note that CONNECT is a subcommand of
   the substructure COMBINE command.

6. The NAMEi's must be the names of basic substructure components of the
   pseudostructures named on the COMBINE card in the Substructure Control
   Deck. See the CONCT card for a more complete discussion related to the
   combination of two substructures.

7. This card and its continuations effectively describe a map of
   connectivities. Grid points entered in the corresponding field of a
   substructure name define the connectivity participation for that
   substructure. Each continuation card defines the connection relationships
   among the participating substructures for the components entered.

8. If GTRAN has been invoked under the COMBINE command, the entries on the
   CONCT and CONCT1 cards must be defined in terms of the revised coordinate
   system.

# CONM1 - Concentrated Mass Element Connection

## Description

Defines a 6x6 symmetric mass matrix at a geometric grid point of the
structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CONM1   |  EID  |   G   |  CID  |  M11  |  M21  |  M22  |  M31  |  M32  |abc  |
|+bc     |  M33  |  M41  |  M42  |  M43  |  M44  |  M51  |  M52  |  M53  |def  |
|+ef     |  M54  |  M55  |  M61  |  M62  |  M63  |  M64  |  M65  |  M66  |     |
|CONM1   |   2   |   22  |   2   |  2.9  |       |  6.3  |       |       |+1   |
|+1      |  4.8  |       |       |       |  28.6 |       |       |       |+2   |
|+2      |       |  28.6 |       |       |       |       |       |  28.6 |     |

Field      Contents
-----      --------
EID        Unique element identification number (Integer > 0).

G          Grid point identification number (Integer > 0).

CID        Coordinate system identification number for the mass matrix
           (Integer >= 0).

Mij        Mass matrix values (Real).

## Remarks

1. For a less general means of defining concentrated mass at grid points, see
   CONM2.

2. Each element identification number must be unique with respect to all other
   element identification numbers.


# CONM2 - Concentrated Mass Element Connection

## Description

Defines a concentrated mass at a grid point of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CONM2   |  EID  |   G   |  CID  |   M   |   X1  |   X2  |   X3  |       |abc  |
|+bc     |  I11  |  I21  |  I22  |  I31  |  I32  |  I33  |       |       |     |
|CONM2   |   2   |   15  |   6   |  49.7 |       |       |       |       |123  |
|+23     | 16.2  |       | 16.2  |       |       |  7.8  |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

G          Grid point identification number (Integer > 0).

CID        Coordinate system identification number (Integer >= 0).

M          Mass value (Real).

X1, X2, X3 Offset distances for the mass in the coordinate system defined in
           field 4 (Real).

Iij        Mass moments of inertia measured at the mass c.g. in coordinate
           system defined by field 4 (Real).

## Remarks

1. Each element identification number must be unique with respect to all other
   element identification numbers.

2. For a more general means of defining concentrated mass at grid points, see
   CONM1.

3. The continuation card may be omitted.

4. The form of the inertia matrix about its c.g. is taken as (see Section
   5.5.2.2 of the Theoretical Manual):

      +                                            +
      | M       0      0    |    0   M(X3) -M(X2)  |
      |         M      0    | -M(X3)  0     M(X1)  |
      |                M    |  M(X2) -M(X1)   0    |
      |                     |                      |
      | --------------------+--------------------- |
      |                     |  BI11  -BI21  -BI31  |
      |       (SYM)         |         BI22  -BI32  |
      |                     |                BI33  |
      +                                            +

   where BI11 = I11 + M(X2**2 + X3**2), BI21 = I21 + (M)(X1)(X2), and BI22,
   BI31, BI32, and BI33 are similarly defined.

# CONROD - Rod Element Property and Connection

## Description

Defines a rod element of the structural model without reference to a property
card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CONROD  |  EID  |  G1   |  G2   |  MID  |   A   |   J   |   C   |  NSM  |     |
|CONROD  |   2   |  16   |  17   |  23   |  2.69 |       |       |       |     |

Field     Contents
-----     --------
EID       Unique element identification number (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

MID       Material identification number (Integer > 0).

A         Area of rod (Real).

J         Torsional constant (Real).

C         Coefficient for torsional stress determination (Real).

NSM       Nonstructural mass per unit length (Real).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.For structural problems, CONROD cards may only reference MAT1 material
  cards.

3.For heat transfer problems, CONROD cards may only reference MAT4 or MAT5
  material cards.


# CORD1C - Cylindrical Coordinate System Definition

## Description

Defines a cylindrical coordinate system by reference to three grid points.
These points must be defined in coordinate systems whose definition does not
involve the coordinate system being defined. The first point is the origin,
the second lies on the z-axis, and the third lies in the plane of the
azimuthal origin.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD1C  |  CID  |  G1   |  G2   |  G3   |  CID  |  G1   |  G2   |  G3   |     |
|CORD1C  |   3   |  16   |  32   |  19   |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

G1, G2, G3Grid point identification numbers (Integer > 0; G1 through G3 must
          be unique).

## Remarks

1.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

2.The three points G1, G2, G3 must be noncollinear.

3.The location of a grid point (P in Figure 2.4-13) in this coordinate system
  is given by (R, é, Z) where é is measured in degrees.

4.The displacement coordinate directions at P are dependent on the location
  of P as shown in Figure 2.4-13 by (ur, ué, uz).

5.Points on the z-axis may not have their displacement directions defined in
  this coordinate system since an ambiguity results.

6.One or two coordinate systems may be defined on a single card.
                                     z     uz
                                     |     |  ué
                                     |     | /
                                     |     |/
                                   G2*   p |\
                                     |     | \
                                    /|     |  \
                                   / |     |   ur
                                  /  |     |Z
                               G3*   |     |
                                 | G1*---------------------y
                                 |  / \    |
                                 | / é \R  |
                                 |/     \  |
                                 /       \ |
                                /         \|
                               /
                              *

                    Figure 2.4-13. CORD1C coordinate system


# CORD1R - Rectangular Coordinate System Definition

## Description

Defines a rectangular coordinate system by reference to three grid points.
These points must be defined in coordinate systems whose definition does not
involve the coordinate system being defined. The first point is the origin,
the second lies on the z-axis, and the third lies in the x-z plane.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD1R  |  CID  |  G1   |  G2   |  G3   |  CID  |  G1   |  G2   |  G3   |     |
|CORD1R  |   3   |  16   |  32   |  19   |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

G1, G2, G3Grid point identification numbers (Integer > 0; G1 through G3 must
          be unique).

## Remarks

1.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

2.The three points G1, G2, G3 must be noncollinear.

3.The location of a grid point (P in Figure 2.4-14) in this coordinate system
  is given by (X, Y, Z).

4.The displacement coordinate directions at P are shown in Figure 2.4-14 by
  (ux, uy, uz).

5.One or two coordinate systems may be defined on a single card.

                                     z
                                     |     uz
                                     |     |
                                     |     | P
                                   G2*     *-------- uy
                                     |    /|
                                    /|   / |
                                   / |  /  | Z
                                  /  | ux  |
                               G3*   |     |
                                 | G1*----------*---------- y
                                 |  /      |   /
                                 | /       |  /
                                 |/        | / X
                                 /         |/
                                /----------*
                               /     Y
                              x

                    Figure 2.4-14. CORD1R coordinate system

# CORD1S - Spherical Coordinate System Definition

## Description

Defines a spherical coordinate system by reference to three grid points. These
points must be defined in coordinate systems whose definition does not involve
the coordinate system being defined. The first point is the origin, the second
lies on the z-axis, and the third lies in the plane of the azimuthal origin.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD1S  |  CID  |  G1   |  G2   |  G3   |  CID  |  G1   |  G2   |  G3   |     |
|CORD1S  |   3   |  16   |  32   |  19   |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

G1, G2, G3Grid point identification numbers (Integer > 0; G1 through G3 must
          be unique).

## Remarks

1.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

2.The three points G1, G2, G3 must be noncollinear.

3.The location of a grid point (P in Figure 2.4-15) in this coordinate system
  is given by (R, é, è) where é and è are measured in degrees.

4.The displacement coordinate directions at P are dependent on the location
  of P as shown in Figure 2.4-15 by (ur, ué, uè).

5.Points on the polar axis may not have their displacement directions defined
  in this coordinate system since an ambiguity results.

6.One or two coordinate systems may be defined on a single card.

                                     z        ur    .uè
                                     |       /   .
                                     |      / .
                                     |    P*
                                   G2*    /| \
                                    /| é / |  \
                                   / |  /  |   ué
                                  /  | /R  |
                               G3*   |/    |
                                 | G1*---------------------y
                                 |  / \    |
                                 | / è \   |
                                 |/     \  |
                                 /       \ |
                                /         \|
                               /
                              x

                    Figure 2.4-15. CORD1S coordinate system


# CORD2C - Cylindrical Coordinate System Definition

## Description

Defines a cylindrical coordinate system by reference to the coordinates of
three points. The first point defines the origin. The second point defines the
direction of the z-axis. The third lies in the plane of the azimuthal origin.
The reference coordinate must be independently defined.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD2C  |  CID  |  RID  |   A1  |   A2  |   A3  |   B1  |   B2  |   B3  |ABC  |
|+BC     |  C1   |  C2   |  C3   |       |       |       |       |       |     |
|CORD2C  |   3   |   17  |  -2.9 |  1.0  |  0.0  |   3.6 |  0.0  |  1.0  |123  |
|+23     |  5.2  |  1.0  |  -2.9 |       |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

RID       Reference to a coordinate system which is defined independently of
          new coordinate system (Integer >= 0 or blank).

A1,A2,A3; B1,B2,B3; C1,C2,C3  Coordinates of three points in coordinate system
          defined in field 3 (Real).

## Remarks

1.Continuation card must be present.

2.The three points (A1, A2, A3), (B1, B2, B3), (C1, C2, C3) must be unique
  and non-collinear. Noncollinearity is checked by the geometry processor.

3.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

4.An RID of zero references the basic coordinate system.

5.The location of a grid point (P in Figure 2.4-16) in this coordinate system
  is given by (R, é, Z) where é is measured in degrees.

6.The displacement coordinate directions at P are dependent on the location
  of P as shown in Figure 2.4-16 by (ur, ué, uz).

7.Points on the z-axis may not have their displacement direction defined in
  this coordinate system, since an ambiguity results.
                                     z     uz
                                     |     |  ué
                                     |     | /
                                     |     |/
                                    B*   p |\
                                     |     | \
                                    /|     |  \
                                   / |     |   ur
                                  /  |     |Z
                                C*   |     |
                                 |  A*---------------------y
                                 |  / \    |
                                 | / é \R  |
                                 |/     \  |
                                 /       \ |
                                /         \|
                               /
                              x

                    Figure 2.4-16. CORD2C coordinate system

# CORD2R - Rectangular Coordinate System Definition

## Description

Defines a rectangular coordinate system by reference to the coordinates of
three points. The first point defines the origin. The second point defines the
direction of the z-axis. The third point defines a vector which, with the
z-axis, defines the x-z plane. The reference coordinate must be independently
defined.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD2R  |  CID  |  RID  |   A1  |   A2  |   A3  |   B1  |   B2  |   B3  |ABC  |
|+BC     |  C1   |  C2   |  C3   |       |       |       |       |       |     |
|CORD2R  |   3   |   17  |  -2.9 |  1.0  |  0.0  |   3.6 |  0.0  |  1.0  |123  |
|+23     |  5.2  |  1.0  |  -2.9 |       |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

RID       Reference to a coordinate system which is defined independently of
          new coordinate system (Integer >= 0 or blank).

A1,A2,A3; B1,B2,B3; C1,C2,C3  Coordinates of three points in coordinate system
          defined in field 3 (Real).

## Remarks

1.Continuation card must be present.

2.The three points (A1, A2, A3), (B1, B2, B3), (C1, C2, C3) must be unique
  and non-collinear. Noncollinearity is checked by the geometry processor.

3.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

4.An RID of zero references the basic coordinate system.

5.The location of a grid point (P in Figure 2.4-17) in this coordinate system
  is given by (X, Y, Z).

6.The displacement coordinate directions at P are shown by (ux, uy, uz).

                                     z
                                     |     uz
                                     |     |
                                     |     | P
                                   B *     *-------- uy
                                     |    /|
                                    /|   / |
                                   / |  /  | Z
                                  /  | ux  |
                               C *   |     |
                                 | A *----------*---------- y
                                 |  /      |   /
                                 | /       |  /
                                 |/        | / X
                                 /         |/
                                /----------*
                               /     Y
                              x

                    Figure 2.4-17. CORD2R coordinate system

# CORD2S - Spherical Coordinate System Definition

## Description

Defines a spherical coordinate system by reference to the coordinates of three
points. The first point defines the origin. The second point defines the
direction of the z-axis. The third lies in the plane of the azimuthal origin.
The reference coordinate must be independently defined.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CORD2S  |  CID  |  RID  |   A1  |   A2  |   A3  |   B1  |   B2  |   B3  |ABC  |
|+BC     |  C1   |  C2   |  C3   |       |       |       |       |       |     |
|CORD2S  |   3   |   17  |  -2.9 |  1.0  |  0.0  |   3.6 |  0.0  |  1.0  |123  |
|+23     |  5.2  |  1.0  |  -2.9 |       |       |       |       |       |     |

Field     Contents
-----     --------
CID       Coordinate system identification number (Integer > 0).

RID       Reference to a coordinate system which is defined independently of
          new coordinate system (Integer >= 0 or blank).

A1,A2,A3; B1,B2,B3; C1,C2,C3  Coordinates of three points in coordinate system
          defined in field 3 (Real).

## Remarks

1.Continuation card must be present.

2.The three points (A1, A2, A3), (B1, B2, B3), (C1, C2, C3) must be unique
  and non-collinear. Noncollinearity is checked by the geometry processor.

3.Coordinate system identification numbers on all CORD1R, CORD1C, CORD1S,
  CORD2R, CORD2C, and CORD2S cards must be unique.

4.An RID of zero references the basic coordinate system.

5.The location of a grid point (P in Figure 2.4-18) in this coordinate system
  is given by (R, é, è) where é and è are measured in degrees.

6.The displacement coordinate directions at P are shown in Figure 2.4-18 by
  (ur, ué, uí).

7.Points on the polar axis may not have their displacement directions defined
  in this coordinate system since an ambiguity results.

                                     z        ur    .uí
                                     |       /   .
                                     |      / .
                                     |    P*
                                   B *    /| \
                                    /| é / |  \
                                   / |  /  |   ué
                                  /  | /R  |
                               C *   |/    |
                                 |  A*---------------------y
                                 |  / \    |
                                 | / í \   |
                                 |/     \  |
                                 /       \ |
                                /         \|
                               /
                              x

                    Figure 2.4-18. CORD2S coordinate system


# CPSEi - Pressure Stiffness Element Connection

## Description

Defines a pressure stiffness element CPSEi (i = 2, 3, 4).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CPSEi   |  EID  |  PID  |   G1  |  G2   |  G3   |  G4   |       |       |     |
|CPSE3   |  34   |   1   |   11  |  10   |  12   |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PPSE property card (Integer > 0).

G1...GN   Grid point identification numbers of connection points (N = 2 for
          CPSE2, N = 3 for CPSE3, and N = 4 for CPSE4).

## Remarks

1.Element identification numbers must be unique with respect to all other
  element identification numbers.

2.These are differential stiffness elements. No structural stiffness and no
  structural mass are generated by these elements.

3.The formulation of these pressure stiffness elements assumes that only the
  basic coordinate system is used to describe the displacement parameters.
  Therefore the grid points, G1...GN, must be in the basic rectangular
  system. (This limitation will be removed later.)

4.All three CPSEi cards share one property card, PPSE.

5.Constant pressure is applied over an enclosed volume encompassed by the
  CPSEi elements. That is, there is no pressure gradient in the enclosed
  space.

6.Pressure acts normally to the CPSEi surfaces.

7.Reference: E. Christensen, "Advanced Solid Rocket Motor (ASRM) Math Models
  - Pressure Stiffness Effects Analysis", Aug. 1991, NASA TD612-001-02.


# CQDMEM - Quadrilateral Element Connection

## Description

Defines a quadrilateral membrane element (QDMEM) of the structural model
consisting of four overlapping TRMEM elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQDMEM  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQDMEM  |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQDMEM property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-19 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-19. CQDMEM sign convention for TH


# CQDMEM1 - Isoparametric Quadrilateral Element Connection

## Description

Defines an isoparametric quadrilateral membrane element (QDMEM1) of the
structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQDMEM1 |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQDMEM1 |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQDMEM1 property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0); G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-20 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

4.In a HEAT formulation, element type CQDMEM1 is automatically replaced by
  element type CQDMEM.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-20. CQDMEM1 sign convention for TH


# CQDMEM2 - Quadrilateral Element Connection

## Description

Defines a quadrilateral membrane element (QDMEM2) of the structural model
consisting of four non-overlapping TRMEM elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQDMEM2 |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQDMEM2 |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQDMEM2 property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-21 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

4.In a HEAT formulation, element type CQDMEM2 is automatically replaced by
  element type CQDMEM.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-21. CQDMEM2 sign convention for TH

# CQDPLT - Quadrilateral Element Connection

## Description

Defines a quadrilateral bending element (QDPLT) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQDPLT  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQDPLT  |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQDPLT property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-22 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

4.No structural mass is generated by this element.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-22. CQDPLT sign convention for TH


# CQUAD1 - Quadrilateral Element Connection

## Description

Defines a quadrilateral membrane and bending element (QUAD1) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQUAD1  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQUAD1  |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQUAD1 property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-23 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-23. CQUAD1 sign convention for TH


# CQUAD2 - Quadrilateral Element Connection

## Description

Defines a homogeneous quadrilateral membrane and bending element (QUAD2) of
the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQUAD2  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TH   |       |     |
|CQUAD2  |   72  |   13  |  13   |  14   |  15   |  16   |  29.2 |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PQUAD2 property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-24 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                 Figure 2.4-24. CQUAD2 sign convention for TH


# CQUAD4 - Quadrilateral Element Connection

## Description

Defines a quadrilateral plate element (QUAD4) of the structural model. This is
an isoparametric membrane-bending element, with variable element thickness,
layered composite material, and thermal analysis capabilities.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CQUAD4  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  TM   |  ZO   | abc |
|+bc     |       |       |  T1   |  T2   |  T3   |  T4   |       |       |     |
|CQUAD4  |  101  |   17  |  1001 |  1005 |  1010 |  1024 |  45.0 |  0.01 | ABC |
|+BC     |       |       |  0.03 | 0.125 |  0.05 |  0.04 |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PSHELL entry (default is EID)
          (Integer > 0). For composites, see Remark 5.

Gi        Grid point identification numbers of connection points
          (Integer > 0).

ZO        Offset of the elementary reference plane (element
          mid-plane) from the plane of grid points (Real or
          blank; see Remark 3 for default; see Guidelines.)

TM        Material property orientation specification (Real or blank,
          or 0 <= Integer < 1,000,000). If Real or blank, specifies
          the material property orientation angle in degrees. If
          Integer, the orientation of the material x-axis is along the
          projection onto the plane of the element of the x-axis of
          the coordinate system specified by the integer value. (See
          Guidelines.)

Ti        Membrane thickness of element at grid points Gi (Real
          or blank; see Remark 4 for default).

## Remarks

1.       The QUAD4 geometry, coordinate systems, and numbering are shown in
         Figure 2.4-25.

                                ye
                                |
                             G4 *---------------* G3
                                |               |
                                |               |
                                |           .   |
                                |        .      |
                                |     .         |
                                |  .            |
                                |   TH          |
                             G1 *---------------*------xe
                                                  G2

                        Figure 2.4-25. CQUAD4 geometry

2.       Each element identification number must be unique with respect to all
         other element identification numbers.

3.       The material coordinate system (TM) and the offset (ZO) may also be
         provided on the PSHELL entry. The PSHELL data will be used if the
         corresponding field on the CQUAD4 entry is blank.

4.       The Ti fields are optional; if not supplied they will be set to the
         value of T specified on the PSHELL entry. In such cases, the
         continuation entry is not required.

5.       For composites, a PCOMP, PCOMP1, PCOMP2 card can be used instead of a
         PSHELL card.

Guidelines for the Use of CQUAD4

(Excerpt from "QUAD4 SEMINAR", WPAFB, WRDC-TR-89-3046, revised April 1993)

QUAD4 is one of the most extensively used elements in NASTRAN. It is a very
versatile element and can be used to model a variety of plate element
applications such as:

         a. Membrane (inplane loading) behavior
         b. Bending (out of plane loading) behavior
         c. Membrane-bending (uncoupled)
         d. Membrane-bending (coupled-linear)
         e. Laminated plates
         f. Layered composites
         g. Sandwich plates with metal face sheets
         h. Sandwich plates with layered composite face sheets
         i. Isotropic materials
         j. Anisotropic (including orthotropic) materials

Application of QUAD4 is often confusing because of the many options available
for its use.

There are five cards which describe the input parameters for QUAD4. They
describe its geometry and properties along with some auxiliary information.

Geometry and Property Cards

CQUAD4                   Connection card
PSHELL                   Property card for homogeneous and sandwich plates
PCOMP, PCOMP1, PCOMP2    Property cards for laminated or layered plates

Material Cards

MAT1                     Isotropic materials
MAT2                     Anisotropic materials
MAT8                     Orthotropic materials
PLOAD4                   Pressure load definition on QUAD4 element

For a given element either the PSHELL or PCOMP card is applicable but not
both. PSHELL cards are for homogeneous (nonlaminated) and sandwich plates with
nonlayered face sheets. PCOMP cards are for laminated (layered) plates. In the
case of sandwich plates with layered face sheets the honeycomb (sandwich) core
will be treated as a laminate or layer.

A supplementary explanation of the parameters on each of these cards should
aid in understanding the modeling nuances of the element.

The definitions of the parameters in fields 2 to 7 are self explanatory and
need no further clarification. Similarly no additional explanation is
necessary for the thickness parameters specified in fields 4 to 7 on the
continuation card. However, the parameters TM and ZO need a supplementary
explanation or caution.

Parameter TM

Parameter TM defines the material property orientation. There are two options
for this definition.

## Option 1

Define the angle between the side of the element (connecting G1 and G2) and
the material axis. This is the least desirable option. It is prone to errors,
because every time the sequence of the element connection changes, the angle
must be changed. Also in a complex three dimensional model it is not easy to
determine this angle without writing a preprocessor.

## Option 2

The integer option is preferable. An integer in field 8 refers to a separate
coordinate system for defining the orientation of the material axis of the
element. The material property definition is now independent of the connection
sequence. The new coordinate system can be defined with a coordinate card.

## Offset Parameter ZO

The offset parameter provision in the QUAD4 element constitutes a significant
enhancement for plate elements. Before, the QUAD4 grid points of the structure
could only be defined on the mid-surface of the plate elements. Bar (beam or
bend) was the only other element with an offset capability. However, some of
the mass elements have the offset capability.

The offset, ZO, is shown for various cases in the diagrams at the end of this
section. Note the distinction between the grid point surfaces and mid-surface
of the element.


SIDE VIEW (PSHELL):
                            TOP                               BOTTOM
                      +--------------+                  +--------------+
CASE 1                |              |                  |              |
                    + - - - - - - - - -               - - - - - - - - - - +
                    | |              |                  |              |  |
                    | +--------------+                  +--------------+  | -ZO
                +ZO |     BOTTOM                              TOP         |
                    |                                                     |
                    + - - - - - - - - - GRID PT SURFACE  - - - - - - -  - +

                            TOP                              BOTTOM
                      +--------------+                  +--------------+
CASE 2                |              |                  |              |
(DEFAULT)     ZO = 0- - - - - - - - --  G.P. SURFACE - -- - - - - - - - - ZO = 0
                      |              |                  |              |
                      +--------------+                  +--------------+
                           BOTTOM                              TOP

                  + - - - - - - - - -  GRID PT SURFACE  - - - - - - - - - +
                  |                                                       |
CASE 3            |                                                       | +ZO
                  |         TOP                              BOTTOM       |
              -ZO |   +--------------+                  +--------------+  |
                  |   |              |                  |              |  |
                  + - - - - - - - - - -               - - - - - - - - - - +
                      |              |                  |              |
                      +--------------+                  +--------------+
                          BOTTOM                               TOP


# CRBAR - Rigid Bar

## Description

Defines a rigid bar with six degrees of freedom at each end.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRBAR   |  EID  |  G1   |  G2   |  IC1  |  IC2  |  DC1  |  DC2  |       |     |
|CRBAR   |   5   |   1   |   2   |  234  |  123  |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

Gi        Identification numbers of connection grid points (Integers > 0).

ICi       Independent degrees of freedom in the global coordinate system for
          the element at grid points Gi (any of the digits 1 - 6 with no
          imbedded blanks. Integers > 0 or blank.) See Remark 2.

DCi       Dependent degrees of freedom in the global coordinate system
          assigned by the element at grid points Gi (any of the digits 1 - 6
          with no imbedded blanks. Integers > 0 or blank.) See Remarks 3 and
          4.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The total number of degrees of freedom specified (IC1 and IC2) must equal
  six; for example, IC1 = 1236, IC2 = 34. Further, they should together be
  capable of representing any general rigid body motion of the element.

3.If both DC1 and DC2 are zero or blank, all of the degrees of freedom not in
  IC1 and IC2 will be made dependent.

4.The dependent (that is, constrained) degrees of freedom in a CRBAR element
  may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

5.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

6.Forces of constraint are not recovered.

7.Rigid elements are ignored in heat transfer problems.

8.NASTRAN actually converts the CRBAR input card into the CRIGD3 card format,
  and thus processes a CRBAR card as if it were a CRIGD3 card. The following
  table shows the method of conversion, in free-field format:

  CRBAR Card                    ===> Equivalent CRIGD3 Card
  __________________________________________________________________

  CRBAR, EID, G1, G2, IC1, IC2, DC1, DC2
                                ===> CRIGD3, EID, G1, IC1, G2, IC2
                                     ,'MSET', G1, DC1, G2, DC2

9.See Section 1.4.2.2 for a discussion of rigid elements.


# CRBE1 - Rigid Body Element, Form 1

## Description

Defines a rigid body connected to an arbitrary number of grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRBE1   |  EID  |  IG1  |  IC1  |  IG2  |  IC2  |  IG3  |  IC3  |       |abc  |
|+bc     |       |  IG4  |  IC4  |  IG5  |  IC5  |  IG6  |  IC6  |       |def  |
|+ef     | "UM"  |  DG1  |  DC1  |  DG2  |  DC2  |  DG3  |  DC3  |       |ghi  |
|+hi     |       |  DG4  |  DC4  |  DG5  |  DC5  |  etc. |       |       |     |
|CRBE1   |  103  |  11   |   1   |  12   |   2   |  13   |   4   |       |ABC  |
|+BC     |       |  14   |  35   |  15   |   6   |       |       |       |CDF  |
|+DF     |  UM   |  21   |  123  |  22   |   1   |  23   | 123456|       |EFI  |
|+FI     |       |  24   |  456  |  25   |   2   |       |       |       |     |


Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IGi       Identification numbers of the reference independent grid points
          (Integers > 0).

ICi       Independent degrees of freedom in the global coordinate system for
          the preceding reference grid point (any of the digits 1 - 6 with
          no imbedded blanks. Integer > 0.) See Remarks 2, 3, and 5.

"UM"      BCD word that indicates the start of the data for dependent grid
          points.

DGi       Identification numbers of the dependent grid points (Integer > 0).

DCi       Dependent degrees of freedom in the global coordinate system for
          the preceding dependent grid point (any of the digits 1 - 6 with
          no imbedded blanks. Integer > 0.) See Remarks 4 and 5.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The total number of degrees of freedom specified for the reference grid
  points (IC1 through IC6) must be six; for example, IC1=1, IC2=2, IC3=4,
  IC4=35, IC5=6. Further, they should together be capable of representing any
  general rigid body motion of the element.

3.The first continuation card is not required if less than four reference
  independent grid points are specified.

4.The dependent (that is, constrained) degrees of freedom in a CRBE1 element
  may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by a MPC card.

5.A degree of freedom cannot be both independent and dependent for the same
  element. However, both independent and dependent components can exist at
  the same grid point.

6.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

7.Forces of constraint are not recovered.

8.Rigid elements are ignored in heat transfer problems.

9.NASTRAN actually converts the CRBE1 input card into the CRIGD3 card format
  by switching the  "UM" BCD word to "MSET", and thus processes a CRBE1 card
  as if it were a CRIGD3 card.

  CRBE1 Card       ===> Equivalent CRIGD3 Card

  CRBE1, EID, IG1, IC1, IG2, IC2, IG3, IC3
  ,'UM', DG1, DC1, DG2, DC2, etc.
                   ===> CRIGD3,  EID, IG1, IC1, IG2, IC2, IG3, IC3
                        ,'MSET', DG1, DC1, DG2, DC2, etc.

10.  See Section 1.4.2.2 for a discussion of rigid elements.

# CRBE2 - Rigid Body Element, Form 2

## Description

Defines a rigid body whose independent degrees of freedom are specified at a
single grid point and whose dependent degrees of freedom are specified at an
arbitrary number of grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRBE2   |  EID  |  IG   |   C   |  G1   |  G2   |  G3   |  G4   |  G5   |abc  |
|+bc     |  G6   |  G7   |  G8   |  etc. |       |       |       |       |     |
|CRBE2   |   9   |   8   |  12   |  10   |  12   |  14   |  15   |  16   |ABC  |
|+BC     |  20   |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IG        Identification number of the reference grid point, to which all
          six independent degrees of freedom for the element are assigned
          (Integer > 0).

C         The dependent degrees of freedom in the global coordinate system
          for all the dependent grid points Gi (any of the digits 1 - 6 with
          no imbedded blanks. Integer > 0.) See Remark 2.

Gi        Identification numbers of the dependent grid points (Integers >
          0).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The dependent (that is, constrained) degrees of freedom in a CRBE2 element
  may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

3.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

4.Forces of constraint are not recovered.

5.Rigid elements are ignored in heat transfer problems.

6.NASTRAN actually converts the CRBE2 input card into the CRIGD2 card format,
  and thus processes a CRBE2 card as if it were a CRIGD2 card. The following
  table shows the method of conversion, in free-field format:

  CRBE2 Card        ===> Equivalent CRIGD2 Card

  CRBE2, EID, IG, C, G1, G2, G3, etc.
                    ===> CRIGD2, EID, IG, G1, C, G2, C, G3, C, etc.

7.See Section 1.4.2.2 for a discussion of rigid elements.


# CRBE3 - Rigid Body Element, Form 3

## Description

Defines the motion at a reference grid point as the weighted average of the
motions at a set of other grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRBE3   |  EID  |       |  IG   |  IC   |  W1   |  C1   | G1,1  | G1,2  |abc  |
|+bc     | G1,3  |  W2   |  C2   |  G2,1 |  G2,2 |  G2,3 |  W3   |  C3   |def  |
|+ef     | G3,1  | G3,2  | G3,3  |  W4   |  C4   | G4,1  | G4,2  | G4,3  |ghi  |
|+hi     | "UM"  |  DG1  |  DC1  |  DG2  |  DC2  |  DG3  |  DC3  |       |jkl  |
|+kl     |       |  DG4  |  DC4  |  DG5  |  DC5  |  DG6  |  DC6  |       |     |
|CRBE3   |  14   |       |  100  | 1234  |  1.0  |  123  |   1   |  3    |ABC  |
|+BC     |  5    |  4.7  |   1   |   2   |   4   |   6   |  5.2  |   2   |DEF  |
|+EF     |  7    |   8   |       |  5.1  |   1   |  15   |  16   |       |GHI  |
|+HI     |  UM   |  100  |  14   |   5   |   3   |   7   |   2   |       |JKL  |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IG        Reference grid point (Integer > 0).

IC        Global components of motion whose values will be computed at the
          reference grid point (any of the digits 1 - 6 with no imbedded
          blanks. Integer > 0).

Wi        Weighting factor for components of motion on the following card at
          grid points Gi,j (Real).

Ci        Global components of motion which have weighting factor Wi at grid
          points Gi,j (any of the digits 1 - 6 with no imbedded blanks.
          Integers > 0).

Gi,j      Grid points whose components Ci have weighting factor Wi in the
          averaging equations (Integers > 0).

"UM"      BCD word that indicates the start of the data for the components
          of motion at grid points DGi (optional). The default is that all
          of the components in IC at the reference grid point IG, and no
          others, are included in the dependent component set {um}.

DGi       Grid points with components DCi in {um} (Integers > 0).

DCi       Components of motion at grid point DGi (any of the digits 1 - 6
          with no imbedded blanks, Integers > 0).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Blank spaces may be left at the end of a Gi,j sequence.

3.The default for UM should be used except in cases where you want to include
  some or all IC components in displacement sets exclusive from the {um} set.
  If the default is not used for UM:

  a. The total number of components in {um} (that is, the total number of
     dependent degrees of freedom defined by the element) must be equal to
     the number of components in IC (four in the above example).

  b. The components in UM must be a subset of the components mentioned in IC
     and (Gi,j; Ci).

  c. The coefficient matrix [Rm] in the constraints equation [Rm]{um} +
     [Rn]{un} = 0 must be nonsingular.

4.The dependent (that is, constrained) degrees of freedom in a CRBE3 element
  may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

5.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

6.Forces of constraint are not recovered.

7.Rigid elements are ignored in heat transfer problems.

8.Unlike the other rigid elements, the CRBE3 element and the CRSPLINE element
  cannot be converted into CRIGD2 or CRIGD3 elements. A FORTRAN subroutine
  (in single precision version and in double precision version) was written
  to handle these two special rigid elements.

# CRIGD1 - Rigid Element Connection

## Description

Defines a rigid element in which all six degrees of freedom of each of the
dependent grid points are coupled to all six degrees of freedom of the
reference grid point.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRIGD1  |  EID  |  IG   |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |  G9   |  etc. |       |       |       |       |     |
|CRIGD1  |  101  |  15   |  18   |  43   |   9   |  26   |  35   |  41   |123  |
|+23     |  8    |  63   |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRIGD1  |  EID  |  IG   | GID1  |"THRU" | GID2  |       |       |       |     |
|CRIGD1  |  201  |  25   |  71   | THRU  |  80   |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IG        Identification number of the reference grid point (Integer > 0).

Gi, GIDi  Identification numbers of the dependent grid points (Integer > 0).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Only one reference grid point is allowed per element. It must appear before
  any of the dependent grid points.

3.Any number of dependent grid points may be specified.

4.When the alternate form is used, no continuation card is permitted and all
  grid points implied by GID1 through GID2 (GID1 < GID2) must exist.

5.Dependent degrees of freedom defined (implicitly) in a RIGD1 element may
  not appear on OMIT, OMIT1, SPC, SPC1, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They also may not appear as
  dependent degrees of freedom in RIGD2, RIGD3, or RIGDR elements or on MPC
  cards.

6.Rigid elements are not allowed in heat transfer analysis.

7.For a discussion of rigid elements, see Section 3.5.6 of the Theoretical
  Manual.


# CRIGD2 - Rigid Element Connection

## Description

Defines a rigid element in which selected degrees of freedom of the dependent
grid points are coupled to all six degrees of freedom of the reference grid
point.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRIGD2  |  EID  |  IG   |  G1   |  C1   |  G2   |  C2   |  G3   |  C3   |abc  |
|+bc     |   G4  |  C4   |  etc. |       |       |       |       |       |     |
|CRIGD2  |  102  |  20   |  9    |  12   |  45   |  123  |  53   |  135  |123  |
|+23     |   27  |  456  |       |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IG        Identification number of the reference grid point (Integer > 0).

Gi        Identification numbers of the dependent grid points (Integer > 0).

Ci        List of selected degrees of freedom associated with the preceding
          dependent grid point (any of the digits 1 - 6 with no imbedded
          blanks).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Only one reference grid point is allowed per element. It must appear before
  the dependent grid point data.

3.Any number of dependent grid points may be specified.

4.Dependent degrees of freedom defined in a RIGD2 element may not appear on
  OMIT, OMIT1, SPC, SPC1, or SUPORT cards, nor may they be redundantly
  implied on ASET or ASET1 cards.  They also may not appear as dependent
  degrees of freedom in RIGD1, RIGD3 or RIGDR elements, or on MPC cards.

5.Rigid elements are not allowed in heat transfer analysis.

6.For a discussion of rigid elements, see Section 3.5.6 of the Theoretical
  Manual.


# CRIGD3 - General Rigid Element Connection

## Description

Defines a rigid element in which selected degrees of freedom of the dependent
grid points are coupled to six selected degrees of freedom at one or more (up
to six) reference grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRIGD3  |  EID  |  IG1  |  IC1  |  IG2  |  IC2  |  IG3  |  IC3  |       |abc  |
|+bc     |       |  IG4  |  IC4  |  IG5  |  IC5  |  IG6  |  IC6  |       |def  |
|+ef     | "MSET"|  DG1  |  DC1  |  DG2  |  DC2  |  DG3  |  DC3  |       |ghi  |
|+hi     |       |  DG4  |  DC4  |  DG5  |  DC5  |  etc. |       |       |     |
|CRIGD3  |  103  |  11   |   1   |  12   |   2   |  13   |   4   |       |ABC  |
|+BC     |       |  14   |  35   |  15   |   6   |       |       |       |DEF  |
|+EF     |  MSET |  21   |  123  |  22   |   1   |  23   | 123456|       |GHI  |
|+HI     |       |  24   |  456  |  25   |   2   |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IGi       Identification numbers of the reference grid points (Integer > 0).

ICi       List of selected degrees of freedom associated with the preceding
          reference grid point (any of the digits 1 - 6 with no imbedded
          blanks).

"MSET"    BCD string that indicates the start of the data for the dependent
          grid points.

DGi       Identification numbers of the dependent grid points (Integer > 0).

DCi       List of selected degrees of freedom associated with the preceding
          dependent grid point (any of the digits 1 - 6 with no imbedded
          blanks).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The total number of degrees of freedom specified for the reference grid
  points (IC1 through IC6) must be six. Further, they should together be
  capable of representing any general rigid body motion of the element.

3.The first continuation card is not required if less than four reference
  gridpoints are specified.

4.The BCD word MSET is required in order to indicate the start of the
  dependent grid point data.

5.Any number of dependent grid points may be specified.

6.Dependent degrees of freedom defined in a RIGD3 element may not appear on
  OMIT, OMIT1, SPC, SPC1, or SUPORT cards, nor may they be redundantly
  implied on ASET or ASET1 cards. They also may not appear as dependent
  degrees of freedom in RIGD1, RIGD2, or RIGDR elements, or on MPC cards.

7.Rigid elements are not allowed in heat transfer analysis.

8.For a discussion of rigid elements, see Section 3.5.6 of the Theoretical
  Manual.


# CRIGDR - Rigid Rod Element Connection

## Description

Defines a pin-ended rod element that is rigid in extension-compression.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRIGDR  |  EID  |   G   |  G1   |  C1   |  EID  |   G   |  G1   |  C1   |     |
|CRIGDR  |  104  |   5   |  9    |  3    |  302  |   12  |   4   |   2   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

G         Identification number of the reference grid point (Integer > 0).

G1        Identification number of the dependent grid point (Integer > 0; G1
          not equal G).

C1        Dependent translational degree of freedom of grid point G1 (1 <=
          Integer <= 3).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Only one reference grid point and only one dependent grid point are allowed
  per element. The two points may not be coincident.

3.The direction represented by the dependent translational degree of freedom
  of the dependent grid point may not be perpendicular or nearly
  perpendicular to the element.

4.One or two RIGDR elements may be defined on a single card.

5.Dependent degrees of freedom defined in a RIGDR element may not appear on
  OMIT, OMIT1, SPC, SPC1, or SUPORT cards, nor may they be redundantly
  implied on ASET or ASET1 cards. They also may not appear as dependent
  degrees of freedom in RIGD1, RIGD2, or RIGD3 elements, or on MPC cards.

6.Rigid elements are not allowed in heat transfer analysis.

7.For a discussion of rigid elements, see Section 3.5.6 of the Theoretical
  Manual.


# CROD - Rod Element Connection

## Description

Defines a tension-compression-torsion element (ROD) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CROD    |  EID  |  PID  |  G1   |  G2   |  EID  |  PID  |  G1   |  G2   |     |
|CROD    |   12  |   13  |  21   |  23   |   3   |   12  |  24   |   5   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PROD property card (default is EID)
          (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.See CONROD for alternative method of rod definition.

3.One or two ROD elements may be defined on a single card.


# CRROD - Rigid Pin-Ended Rod

## Description

Defines a pin-ended rod that is rigid in extension.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRROD   |  EID  |   G1  |  G2   |  C1   |  C2   |       |       |       |     |
|CRROD   |   14  |   1   |  2    |  2    |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

Gi        Identification numbers of connection grid points (Integers > 0).

Ci        Component number of one and only one dependent translational
          degree of freedom in the global coordinate system assigned to
          either G1 or G2. (Integer equals 1, 2, or 3.) Either C1 or C2 must
          contain an integer and the other must be blank. See Remarks 2 and
          3.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The grid point that associates with a blank Ci field is designated as the
  reference independent grid point.

3.The dependent (that is, constrained) degrees of freedom in a CRROD element
  may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they be
  redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

4.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

5.Forces of constraint are not recovered.

6.Rigid elements are ignored in heat transfer problems.

7.The degree of freedom selected to be dependent must have a nonzero
  component along the axis of the rod.

8.NASTRAN actually converts the CRROD input card into the CRIGDR card format,
  and thus processes a CRROD card as if it were a CRIGDR card. The following
  table shows the conversion, in free-field format, of two possible cases:

  Case  CRROD Card                 ===>  Equivalent CRIGDR Card

   1    CRROD, EID, G1, G2, C1,    ===>  CRIGDR, EID, G2, G1, C1
   2    CRROD, EID, G1, G2,   , C2 ===>  CRIGDR, EID, G1, G2, C2

9.See Section 1.4.2.2 for a discussion of rigid elements.


# CRSPLINE - Interpolation Constraint Element

## Description

Defines multipoint constraints for the interpolation of displacements at grid
points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRSPLINE|  EID  |  D/L  |  G1   |  G2   |  C2   |  G3   |  C3   |  G4   |abc  |
|+bc     |  C4   |   G5  |  C5   |  G6   |  etc. |       |       |       |     |
|CRSPLINE|   73  |  .05  |  27   |  28   | 123456|  29   |       |  30   |ABC  |
|+BC     |  123  |   75  |  123  |  71   |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

D/L       Ratio of the diameter of the elastic tube which the spline
          represents to the sum of the lengths of all segments. Default =
          0.1 (Real > 0.).

Gi        Identification number of the ith grid point (Integer > 0).

Ci        Components to be constrained at the ith grid point (any of the
          digits 1 - 6 with no imbedded blanks, or blank). See Remark 3.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Displacements are interpolated from the equations of an elastic beam
  passing through the grid points.

3.A blank entry in Ci indicates that all six degrees of freedom at Gi are
  independent. Since G1 must be independent, no field is provided for C1.
  Since the last grid point must also be independent, the last entry must be
  a Gi, not a Ci. For the example shown, G1, G3, and G6 are independent; G2
  has six constrained degrees of freedom while G4 and G5 each have three.

4.The dependent (that is, constrained) degrees of freedom in a CRSPLINE
  element may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they
  be redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

5.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

6.Forces of constraint are not recovered.

7.Rigid elements are ignored in heat transfer problems.

8.This CRSPLINE is not really a rigid element in the normal sense, and should
  not be used for other than its intended purpose.

9.Unlike the other rigid elements, this CRSPLINE element and the CRBE3
  element cannot be converted into CRIGD2 or CRIGD3 elements. A FORTRAN
  subroutine (in single precision version and in double precision version)
  was written to handle these two special rigid elements.


# CRTRPLT - Rigid Triangular Plate

## Description

Defines a rigid triangular plate.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CRTRPLT |  EID  |  G1   |  G2   |  G3   |  IC1  |  IC2  |  IC3  |       |abc  |
|+bc     |  DC1  |  DC2  | DC3   |       |       |       |       |       |     |
|CRTRPLT |   7   |   1   |   2   |   3   |  1236 |   3   |   3   |       |ABC  |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

Gi        Identification numbers of the triangular plate grid points.
          (Integers > 0).

ICi       Independent degrees of freedom in the global coordinate system for
          the element at grid points Gi (any of the digits 1 - 6 with no
          imbedded blanks. Integers > 0 or blank.) See Remark 2.

DCi       Dependent degrees of freedom in the global coordinate system (any
          of the digits 1 - 6 with no imbedded blanks. Integers > 0 or
          blank.) See Remarks 3 and 4.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The total number of degrees of freedom specified for the reference grid
  points (IC1, IC2, and IC3) must be six; for example, IC1 = 1236, IC2 = 3,
  IC3 = 3. Further, they should together be capable of representing any
  general rigid body motion of the element.

3.If DC1, DC2, and DC3 are all zero or blank or if the continuation card is
  omitted, all of the degrees of freedom not in IC1, IC2, and IC3 will be
  made dependent.

4.The dependent (that is, constrained) degrees of freedom in a CRTRPLT
  element may not appear on OMIT, OMIT1, SPC, or SUPORT cards, nor may they
  be redundantly implied on ASET or ASET1 cards. They may not appear as
  dependent degrees of freedom in other rigid elements or on MPC cards.
  Degrees of freedom declared to be independent by a rigid element can be
  made dependent by another rigid element or by an MPC card.

5.Rigid elements, unlike MPCs, are not selected through the Case Control
  Deck.

6.Forces of constraint are not recovered.

7.Rigid elements are ignored in heat transfer problems.

8.NASTRAN actually converts the CRTRPLT input card into the CRIGD3 card
  format, and thus processes a CRTRPLT card as if it were a CRIGD3 card. The
  following table shows the method of conversion, in free-field format:

  CRTRPLT Card            ===> Equivalent CRIGD3 Card

  CRTRPLT, EID, G1,  G2, G3, IC1, IC2, IC3
         , DC1, DC2, DC3
                          ===> CRIGD3, EID, G1, IC1, G2, IC2, G3, IC3
                               ,'MSET', G1, DC1, G2, DC2, G3, DC3

9.See Section 1.4.2.2 for a discussion of rigid elements.


# CSHEAR - Shear Panel Element Connection

## Description

Defines a shear panel element (SHEAR) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CSHEAR  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |       |       |     |
|CSHEAR  |   3   |   6   |   1   |   5   |   3   |   7   |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PSHEAR property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.


# CSLOTi - Slot Element Connections

## Description

Defines an element connecting i = 3 or i = 4 points which solves the wave
equation in two dimensions. Used in acoustic cavity analysis for the
definition of evenly spaced radial slots.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CSLOT3  |  EID  |  IDS1 |  IDS2 |  IDS3 |       |  RHO  |   B   |   M   |     |
|CSLOT3  |  100  |   1   |   3   |   2   |       | 3.E-3 |       |   6   |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CSLOT4  |  EID  |  IDS1 |  IDS2 |  IDS3 |  IDS4 |  RHO  |   B   |   M   |     |
|CSLOT4  |  101  |   1   |   3   |   2   |   4   |       | 6.2+4 |   3   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

IDSj      Identification number of connected grid points, j = 1,2,...J
          (Integer > 0).

RHO       Fluid density in mass units (Real > 0.0 or blank).

B         Fluid bulk modulus (Real >= 0.0 or blank).

M         Number of slots in circumferential direction (Integer >= 0, or
          blank).

## Remarks

1.This card is allowed only if an AXSLOT card is also present.

2.The element identification number (EID) must be unique with respect to all
  other fluid or structural elements.

3.If RHO, B, or M is blank, the corresponding values on the AXSLOT data card
  are used, in which case the default value must not be blank (undefined).

4.Plot elements connecting two points at a time are generated for these
  elements. The CSLOT3 element generates three plot elements. The CSLOT4
  element generates four plot elements, connecting points 1-2, 2-3, 3-4, and
  4-1.

5.If B = 0.0 the slot is considered to be an incompressible fluid.

6.If M = 0 no matrices for CSLOTi elements are generated.


# CTETRA - Tetrahedron Element Connection

## Description

Defines a tetrahedron element (3 dimensional solid with 4 vertices and 4
triangular faces, TETRA) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTETRA  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |       |       |     |
|CTETRA  |  15   |   2   |   4   |   7   |   9   |  11   |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

MID       Material identification number (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integers >
          0, G1 through G4 must be unique). See Figure 2.4-26.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.There is no nonstructural mass.

3.For structural problems, material must be defined by MAT1 card.

4.Output stresses are given in basic coordinate system.

5.For heat transfer problems, material may be defined with either a MAT4 or
  MAT5 card.

                                         G4
                                         *
                                        /|\
                                       / | \
                                      /  |  \
                                     /   |   \
                                  G1*    |    \
                                    \ .  |     \
                                     \  .|      \
                                      \  | .     \
                                       \ |    .   \
                                         *---------*
                                        G2          G3

            Figure 2.4-26. CTETRA grid point identification numbers


# CTORDRG - Toroidal Ring Element Connection
## Description

Defines an axisymmetric toroidal cross-section ring element (TORDRG) of the
structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTORDRG |  EID  |  PID  |  G1   |  G2   |  A1   |  A2   |       |       |     |
|CTORDRG |  25   |   2   |  47   |  48   |  30.0 |  60.0 |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Property identification number (default is EID) (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

A1        Angle of curvature at grid point 1 in degrees (Real; 0 degrees <=
          A1 <= 180 degrees; A2 >= A1).

A2        Angle of curvature at grid point 2 in degrees (Real; 0 degrees <=
          A2 <= 180 degrees; A2 >= A1). See Figure 2.4-27.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 and G2 must lie in the x-z plane of the basic coordinate
  system and to the right of the axis of symmetry (the z-axis).

3.If A1 = 0, the element is assumed to be a shell cap.

4.Only elements of zero or positive Gaussian curvature may be used.

                       z
                       |
                       |       |A1/Surface
                       |       | / Normal
              Axis     |       |/
               of      |       *
            Symmetry   |       G1        |
                       |                 |
                       |                 | A2
                       |                 *--------
                       |                 G2     Surface
                       |                        Normal
                       +-------------------------------------- x

           Figure 2.4-27. CTORDRG grid point identification numbers

# CTRAPAX - Trapezoidal Ring Element Connection

## Description

Defines an axisymmetric trapezoidal cross-section ring element with
non-axisymmetric deformation of the structural model with reference to
property card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRAPAX |  EID  |  PID  |  R1   |  R2   |  R3   |  R4   |  TH   |       |     |
|CTRAPAX |  15   |   5   |  10   |  11   |  12   |  13   | 30.0  |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRAPAX card (Integer > 0).

R1, R2, R3, R4  Identification numbers of RINGAX cards (Integer > 0; R1
          through R4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-28 gives the sign convention for TH.

## Remarks

1.CTRAPAX card is allowed if and only if an AXIC card is also present.

2.Each element identification number must be unique with respect to all other
  element identification numbers.

3.RINGAX identification numbers R1, R2, R3, and R4 must be ordered
  counterclockwise around the perimeter.

4.For a discussion of the axisymmetric ring problem, see Section 5.11 of the
  Theoretical Manual.

5.The lines connecting R1 to R2 and R4 to R3 must be parallel to the r axis.

6.This element cannot be modeled with a grid point on the axis of symmetry.

                 z
                 |
                 |       |
                 |       |
                 |       |
                 |       |   R4 *-------------* R3
       Axis      |       |     /               \
        of       |       |    /         .       \
     Symmetry    |       |   /       .           \
                 |       |  /     .               \
                 |       | /   .    TH             \
                 |       |/ .                       \
                 |    R1 *---------------------------* R2
                 |
                 |
                 +--------------------------------------------------------- r

                 Figure 2.4-28. CTRAPAX sign convention for TH

# CTRAPRG - Trapezoidal Ring Element Connection

## Description

Defines an axisymmetric trapezoidal cross-section ring element (TRAPRG) of the
structural model without reference to a property card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRAPRG |  EID  |  G1   |  G2   |  G3   |  G4   |  TH   |  MID  |       |     |
|CTRAPRG |   72  |  13   |  14   |  15   |  16   | 29.2  |  13   |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

G1,...,G4 Grid point identification number of connection points (Integers >
          0; G1 through G4 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-29 gives the sign convention for TH.

MID       Material property identification number (Integer > 0).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The four grid points must lie in the x-z plane of both the basic and any
  local coordinate systems and to the right of the axis of symmetry (the
  z-axis), except that the grid points G1 and G4 may lie on the axis of
  symmetry in the limiting case when the element becomes a solid core
  element. (See Section 1.3.7.1.)

3.Grid points G1, G2, G3, and G4 must be ordered counterclockwise around the
  perimeter of the element as in the above sketch.

4.The line connecting grid points G1 and G2 and the line connecting grid
  points G3 and G4 must both be parallel to the x-axis.

5.All interior angles must be less than 180 degrees.

6.For structural problems, the material property identification number must
  reference only a MAT1 or MAT3 card.

7.For heat transfer problems, the material property identification number
  must reference only a MAT4 or MAT5 card.

                 z
                 |
                 |       |
                 |       |
                 |       |
                 |       |   G4 *-------------* G3
       Axis      |       |     /               \
        of       |       |    /         .       \
     Symmetry    |       |   /       .           \
                 |       |  /     .               \
                 |       | /   .    TH             \
                 |       |/ .                       \
                 |    G1 *---------------------------* G2
                 |
                 |
                 +--------------------------------------------------------- x

           Figure 2.4-29. CTRAPRG grid point identification numbers


# CTRBSC - Triangular Element Connection

## Description

Defines a basic triangular bending element (TRBSC) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRBSC  |  EID  |  PID  |  G1   |  G2   |  G3   |  TH   |       |       |     |
|CTRBSC  |  16   |   2   |  12   |   1   |   3   | 16.2  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRBSC property card (default is EID)
          (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integer >
          0; G1 through G3 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-30 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

3.No structural mass is generated by this element.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                 Figure 2.4-30. CTRBSC sign convention for TH


# CTRIA1 - Triangular Element Connection

## Description

Defines a triangular membrane and bending element (TRIA1) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIA1  |  EID  |  PID  |  G1   |  G2   |  G3   |  TH   |       |       |     |
|CTRIA1  |  16   |   2   |  12   |   1   |   3   | 16.2  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRIA1 property card (default is EID)
          (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integer >
          0; G1 through G3 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-31 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                 Figure 2.4-31. CTRIA1 sign convention for TH


CTRIA2 - Triangular Element Connection
======================================

## Description

Defines a triangular membrane and bending element (TRIA2) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIA2  |  EID  |  PID  |  G1   |  G2   |  G3   |  TH   |       |       |     |
|CTRIA2  |  16   |   2   |  12   |   1   |   3   | 16.2  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRIA2 property card (default Is EID)
          (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integer >
          0; G1 through G3 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-32 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                 Figure 2.4-32. CTRIA2 sign convention for TH


CTRIA3 - Triangular Element Connection
======================================

## Description

Defines a triangular plate element (CTRIA3) of the structural model. This is
an isoparametric membrane-bending element, with variable element thickness,
layered composite material, and thermal analysis capabilities.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIA3  |  EID  |  PID  |  G1   |  G2   |  G3   |  TM   |  ZO   |       |abc  |
|+bc     |       |       |  T1   |  T2   |  T3   |       |       |       |     |
|CTRIA3  |  101  |  17   | 1001  | 1005  | 1010  | 45.0  | 0.01  |       |ABC  |
|+BC     |       |       | 0.03  | 0.125 | 0.05  |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PSHELL entry (default is EID) (Integer
          > 0). For composites, see Remark 5.

Gi        Grid point identification numbers of connection points (Integer >
          0).

ZO        Offset of the elementary reference plane (element mid-plane) from
          the plane of grid points (Real or blank; see Remarks 3 and 6).

TM        Material property orientation specification (Real or blank, or 0
          <= Integer < 1,000,000). If Real or blank, specifies the material
          property orientation angle in degrees. If Integer, the orientation
          of the material x-axis is along the projection onto the plane of
          the element of the x-axis of the coordinate system specified by
          the integer value. (See Guidelines.)

Ti        Membrane thickness of element at grid points Gi (Real or blank;
          see Remark 4 for default).

## Remarks

1.The TRIA3 geometry, coordinate systems, and numbering are shown in Figure
  2.4-33.

2.Each element identification number must be unique with respect to all other
  element identification numbers.

3.The material coordinate system (TM) and the offset (ZO) may also be
  provided on the PSHELL entry. The PSHELL data will be used if the
  corresponding field on the CTRIA3 entry is blank.

4.The Ti are optional; if not supplied they will be set to the value of T
  specified on the PSHELL entry. In such cases, the continuation entry is not
  required.

5.For composites, a PCOMP, PCOMP1, PCOMP2 card can be used instead of a
  PSHELL card.

6.The "Guidelines for the Use of CQUAD4" and "Guidelines for the use of PCOMP,
  PCOMP1, and PCOMP2" are also applicable to this CTRIA3 element.

7.IMPORTANT: One-third of the CTRIA3 element mass is distributed to each grid
  point if the element has uniform thickness, disregarding the geometry of
  the element (same distribution as CTRIA1 and CTRIA2 elements). This
  one-third mass distribution formulation also exerts heavy influence on an
  element with variable thickness.

8.IMPORTANT: If PLOAD2 or PLOAD4 are applied to the CTRIA3 element, the total
  pressure load is evenly distributed to the three grid points (same result
  when PLOAD2 is applied to CTRIA1 or CTRIA2 element). PLOAD2 can be used
  with the 88th word of SYSTEM set to 1 to distribute load more correctly.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                        Figure 2.4-33. CTRIA3 geometry

CTRIAAX - Triangular Ring Element Connection
============================================

## Description

Defines an axisymmetric triangular cross-section ring element with
non-axisymmetric deformation of the structural model with reference to
property card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIAAX |  EID  |  PID  |  R1   |  R2   |  R3   |  TH   |       |       |     |
|CTRIAAX |  20   |   15  |  42   |  43   |  52   | 60.0  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRIAAX card (Integer > 0).

R1, R2, R3Identification numbers of RINGAX cards (Integer > 0; R1 through R3
          must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-34 gives the sign convention for TH.

## Remarks

1.The CTRIAAX card is allowed if and only if an AXIC card is also present.

2.Each element identification number must be unique with respect to all other
  element identification numbers.

3.RINGAX identification numbers R1, R2, and R3 must be ordered
  counterclockwise around the perimeter.

4.For a discussion of the axisymmetric ring problem, see Section 5.11 of the
  Theoretical Manual.

                      z
                      |            *R3
                      |    |      / \
                      |    |     /   \      .
       Axis           |    |    /     \  .
        of            |    |   /      .\
    Symmetry          |    |  /    .    \
                      |    | /  .    TH  \
                      |    |/.            \
                      |    *---------------*
                      |    R1              R2
                      |
                      +--------------------------------- r

                 Figure 2.4-34. CTRIAAX sign convention for TH


CTRIARG - Triangular Ring Element Connection
============================================

## Description

Defines an axisymmetric triangular cross section ring element (TRIARG) of the
structural model without reference to a property card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIARG |  EID  |  G1   |  G2   |  G3   |  TH   |  MID  |       |       |     |
|CTRIARG |   16  |  12   |  13   |  14   | 29.2  |  17   |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integers >
          0; G1 through G3 must be unique). See Figure 2.4-35.

TH        Material property orientation angle in degrees (Real). Figure
          2.4-35 gives the sign convention for TH.

MID       Material identification number (Integer 0).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The grid points must lie in the x-z plane of both the basic and any local
  coordinate systems and to the right of the axis of symmetry (the z-axis).

3.Grid points G1, G2, and G3 must be ordered counterclockwise around the
  perimeter of the element as shown in the above sketch.

4.For structural problems, the material property identification number must
  reference only a MAT1 or MAT3 card.

5.For heat transfer problems, the material property identification number
  must reference only a MAT4 or MAT5 card.

                      z
                      |            *G3
                      |    |      / \
                      |    |     /   \      .
       Axis           |    |    /     \  .
        of            |    |   /      .\
    Symmetry          |    |  /    .    \
                      |    | /  .    TH  \
                      |    |/.            \
                      |    *---------------*
                      |    G1              G2
                      |
                      +--------------------------------- x

           Figure 2.4-35. CTRIARG grid point identification numbers


CTRIM6 - Linear Strain Triangular Element Connection
====================================================

## Description

Defines a linear strain triangular membrane element (TRIM6) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRIM6  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |+abc |
|+abc    |  TH   |       |       |       |       |       |       |       |     |
|CTRIM6  |  220  |  666  | 100   |  110  |  120  |  210  |  220  |  320  | AC3 |
|+C3     |  90.0 |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of PTRIM6 property card (default is EID)
          (Integer > 0).

G1,...,G6 Grid point identification numbers of connection points (Integers >
          0); G1 through G6 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-36 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

3.The grid points must be ordered consecutively around the perimeter in a
  counterclockwise direction and starting at a vertex.

4.If MAT2 card is used, material properties and stresses are given in the
  material coordinate system.

5.The continuation card must be present.

6.Grid points G2, G4, and G6 are assumed to lie at the midpoints of the
  sides. The locations of these points (defined by GRID cards) are used only
  for the global coordinate system definition, the Grid Point Weight
  Generator, centrifugal forces, and deformed structure plotting.

                                   *G5
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           | G6*      .*G4
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - -*- - - -* -------xe
                           G1      G2      G3

                 Figure 2.4-36. CTRIM6 sign convention for TH


CTRMEM - Triangular Element Connection
======================================

## Description

Defines a triangular membrane element (TRMEM) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRMEM  |  EID  |  PID  |  G1   |  G2   |  G3   |  TH   |       |       |     |
|CTRMEM  |  16   |   2   |  12   |   1   |   3   | 16.3  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRMEM property card (default is EID)
          (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integer >
          0; G1 through G3 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-37 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                 Figure 2.4-37. CTRMEM sign convention for TH


CTRPLT - Triangular Element Connection
======================================

## Description

Defines a triangular bending element (TRPLT) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRPLT  |  EID  |  PID  |  G1   |  G2   |  G3   |  TH   |       |       |     |
|CTRPLT  |  16   |   2   |  12   |   1   |   3   | 16.2  |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTRPLT property card (default is EID)
          (Integer > 0).

G1, G2, G3Grid point identification numbers of connection points (Integer >
          0; G1 through G3 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-38 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

3.No structural mass is generated by this element.

                                   *G3
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           |   /      .\
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - - - - - -* -------xe
                           G1              G2

                 Figure 2.4-38. CTRPLT sign convention for TH


CTRPLT1 - Triangular Element Connection
=======================================

## Description

Defines a higher order triangular bending element (TRPLT1) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRPLT1 |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  TH   |       |       |       |       |       |       |       |     |
|CTRPLT1 |  160  |   20  |  120  |  10   |  30   |  40   |  70   |  110  |ABC  |
|+BC     |  16.2 |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of PTRPLTI property card (default is EID) 
          (Integer > 0).

G1,...,G6 Grid point identification numbers of connection points (Integer >
          0; G1 through G6 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-39 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

3.The grid points must be ordered consecutively around the perimeter in
  counterclockwise direction and starting at a vertex.

4.The continuation card is required.

                                   *G5
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           | G6*      .*G4
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - -*- - - -* -------xe
                           G1      G2      G3

                 Figure 2.4-39. CTRPLT1 sign convention for TH


CTRSHL - Triangular Shell Element Connection
============================================

## Description

Defines a triangular thin shallow shell element (TRSHL) of the structural
model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTRSHL  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  TH   |       |       |       |       |       |       |       |     |
|CTRSHL  |  160  |   20  |  120  |  10   |  30   |  40   |  70   |  110  |ABC  |
|+BC     |  16.2 |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of PTRSHL property card (default is EID) 
          (Integer > 0).

G1,...,G6 Grid point identification numbers of connection points (Integers >
          0; G1 through G6 must be unique).

TH        Material property orientation angle in degrees (Real). Figure
          2.4-40 gives the sign convention for TH.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Interior angles must be less than 180 degrees.

3.The grid points must be listed consecutively around the perimeter in
  counterclockwise direction and starting at a vertex.

4.The continuation card must be present.

                                   *G5
                                  / \
                           ye    /   \      .
                           |    /     \  .
                           | G6*      .*G4
                           |  /    .    \
                           | /  .    TH  \
                           |/.            \
                           *- - - -*- - - -* -------xe
                           G1      G2      G3

                 Figure 2.4-40. CTRSHL sign convention for TH

CTUBE - Tube Element Connection
===============================

## Description

Defines a tension-compression-torsion element (TUBE) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTUBE   |  EID  |  PID  |  G1   |  G2   |  EID  |  PID  |  G1   |  G2   |     |
|CTUBE   |   12  |   13  |  21   |  23   |   3   |   12  |  24   |   5   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTUBE property card (default is EID)
          (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.One or two TUBE elements may be defined on a single card.


CTWIST - Twist Panel Element Connection
=======================================

## Description

Defines a twist panel element (TWIST) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CTWIST  |  EID  |  PID  |  G1   |  G2   |  G3   |  G4   |       |       |     |
|CTWIST  |   2   |   6   |  1    |  5    |   3   |  7    |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

PID       Identification number of a PTWIST property card (default is EID)
          (Integer > 0).

G1,...,G4 Grid point identification numbers of connection points (Integer >
          0; G1 through G4 must be unique).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.Grid points G1 through G4 must be ordered consecutively around the
  perimeter of the element.

3.All interior angles must be less than 180 degrees.


CVISC - Viscous Damper Connection
=================================

## Description

Defines a viscous damper element (VISC) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CVISC   |  EID  |  PID  |  G1   |  G2   |  EID  |  PID  |  G1   |  G2   |     |
|CVISC   |   21  |  6327 |  29   |  31   |  22   |  6527 |  35   |  33   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > O).

PID       Identification number of PVISC property card (default is EID)
          (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.One or two VISC elements may be defined on a single card.

3.Used only for direct formulation of dynamic analyses.


CWEDGE - Wedge Element Connection
=================================

## Description

Defines a wedge element (three dimensional solid, with three quadrilateral
faces and two opposing triangular faces, WEDGE) of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CWEDGE  |  EID  |  MID  |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |     |
|CWEDGE  |   15  |   2   |   3   |   6   |   9   |  12   |  15   |  18   |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

MID       Material identification number (Integer > 0).

G1,...,G6 Grid point identification numbers of connection points (Integers >
          0, G1 through G6 must be unique). See Figure 2.4-41.

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.The order of the grid points is: G1, G2, G3 on one triangular face, G4, G5,
  G6 at the other triangular face. G1, G4 on a common edge, G2, G5 on a
  common edge.

3.The quadrilateral faces must be nearly planar.

4.There is no nonstructural mass.

5 For structural problems, material must be defined by MAT1 card.

6.Output stresses are given in the basic coordinate system.

7.For heat transfer problems, material may be defined with either a MAT4 or
  MAT5 card.

                                     G4_____G6
                                      /\   /\
                                     /  \ /  \
                                    /    *G5  \
                                 G1*- - -|- - -*G3
                                    \    |    /
                                     \   |   /
                                      \  |  /
                                       \ | /
                                         *
                                         G2

            Figure 2.4-41. CWEDGE grid point identification numbers


CYJOIN - Cyclic Symmetry Boundary Points
========================================

## Description

Defines the boundary points of a segment for cyclic symmetry structural
models.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CYJOIN  |  SIDE |   C   |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |abc  |
|+bc     |  G7   |  G8   |  G9   | etc.  |       |       |       |       |     |
|CYJOIN  |   1   |       |   7   |   9   |  16   |  25   |  33   |  64   |ABC  |
|+BC     |  72   |       |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|CYJOIN  |  SIDE |   C   |  GID1 |"THRU" |  GID2 |       |       |       |     |
|CYJOIN  |   2   |   S   |   6   | THRU  |  32   |       |       |       |     |

Field     Contents
-----     --------
SIDE      Side identification (Integer 1 or 2).

C         Coordinate system (BCD value R, C, or S, or blank).

Gi, GIDi  Grid or scalar point identification numbers (Integer > 0).

## Remarks

1.CYJOIN bulk data cards are only used for cyclic symmetry problems. A
  parameter (CTYPE) must specify rotational or dihedral symmetry.

2.For rotational symmetry problems there must be one logical card for side 1
  and one for side 2. The two lists specify grid points to be connected,
  hence both lists must have the same length.

3.For dihedral symmetry problems, side 1 refers to the boundary between
  segments and side 2 refers to the middle of a segment. A coordinate system
  must be referenced in field 3, where R = rectangular, C = cylindrical, and
  S = spherical.

4.All components of displacement at boundary points are connected to adjacent
  segments, except those constrained by SPC, MPC, or OMIT.


DAREA - Dynamic Load Scale Factor
=================================

## Description

The DAREA card is used in conjunction with the RLOAD1, RLOAD2, TLOAD1, and
TLOAD2 data cards and defines the point where the dynamic load is to be
applied with the scale (area) factor A.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DAREA   |  SID  |   P   |   C   |   A   |   P   |   C   |   A   |       |     |
|DAREA   |   3   |   6   |   2   |  8.2  |   15  |   1   |  10.1 |       |     |

Field     Contents
-----     --------
SID       Identification number of DAREA set (Integer > 0).

P         Grid or scalar point identification number (Integer > 0).

C         Component number (1 - 6 for grid point: blank or 0 for scalar
          point).

A         Scale (area) factor A for the designated coordinate (Real).

## Remarks

1.One or two scale factors may be defined on a single card.

2.For axisymmetric problems, P represents the NASTRAN (or internal) grid ID
  and is given by the following algorithm:

  P = Your (or external) ring ID + 10**6 x (harmonic + 1)

DAREAS - Dynamic Load Scale Factor, Substructure Analysis
=========================================================

## Description

The DAREAS card is used in conjunction with the RLOAD1, RLOAD2, TLOAD1, and
TLOAD2 data cards and defines the point where the dynamic load is to be
applied with the scale (area) factor A.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DAREAS  |  SID  |  NAME |   P   |   C   |   A   |   P   |   C   |   A   |     |
|DAREAS  |   3   |  SKIN |   6   |   2   |  8.2  |   15  |   1   |  10.1 |     |

Field     Contents
-----     --------
SID       Identification number of DAREA set (Integer >  0).

NAME      Basic substructure name.

P         Grid or scalar point identification number (Integer > 0).

C         Component number (1 - 6 for grid point; blank or 0 for scalar
          point).

A         Scale (area) factor A for the designated coordinate (Real).

## Remarks

1.One or two scale factors may be defined on a single card.

2.Used in substructure SOLVE operation.

3.Points referenced must exist in the SOLVEd structure.


DEFORM - Element Deformation
============================

## Description

Defines enforced axial deformation for one-dimensional elements for use in
statics problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DEFORM  |  SID  |  EID  |   D   |  EID  |   D   |  EID  |   D   |       |     |
|DEFORM  |   1   |  535  |  .05  |  536  | -.10  |       |       |       |     |

Field     Contents
-----     --------
SID       Deformation set identification number (Integer > 0).

EID       Element number (Integer > 0).

D         Deformation (+ = elongation) (Real).

## Remarks

1.The referenced element must be one-dimensional (that is, a ROD (including
  CONROD), TUBE, or BAR).

2.Deformation sets must be selected in the Case Control Deck (DEFORM = SID)
  to be used by NASTRAN.

3.From one to three enforced element deformations may be defined on a single
  card.


DELAY - Dynamic Load Time Delay
===============================

## Description

The DELAY card is used in conjunction with the RLOAD1, RLOAD2, TLOAD1 and
TLOAD2 data cards and defines the time delay term ç in the equations of the
loading function.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DELAY   |  SID  |   P   |   C   |   T   |   P   |   C   |   T   |       |     |
|DELAY   |   5   |   21  |   6   |  4.25 |   7   |   6   |  8.1  |       |     |

Field     Contents
-----     --------
SID       Identification number of DELAY set (Integer > 0).

P         Grid or scalar point identification number (Integer > 0).

C         Component number (1 - 6 for grid point, blank or 0 for scalar
          point).

T         Time delay ç for designated coordinate (Real).

## Remarks

1.One or two dynamic load time delays may be defined on a single card.


DELAYS - Dynamic Load Time Delay, Substructure Analysis
=======================================================

## Description

The DELAYS card is used in conjunction with the RLOAD1, RLOAD2, TLOAD1 and
TLOAD2 data cards and defines the time delay term ç in the equations of the
loading function.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DELAYS  |  SID  |  NAME |   P   |   C   |   T   |   P   |   C   |   T   |     |
|DELAYS  |   5   |  SKIN |   21  |   6   |  4.25 |   7   |   6   |  8.1  |     |

Field     Contents
-----     --------
SID       Identification number of DELAY set (Integer > 0).

NAME      Basic substructure name.

P         Grid or scalar point identification number (Integer > 0).

C         Component number (1 - 6 for grid point, blank or 0 for scalar
          point).

T         Time delay ç for designated coordinate (Real).

## Remarks

1.One or two dynamic load time delays may be defined on a single card.

2.Used in substructure SOLVE operation.

3.Points referenced must exist in the SOLVEd structure.


DLOAD - Dynamic Load Combination (Superposition)
================================================

## Description

Defines a dynamic loading condition for frequency response or transient
response problems as a linear combination of load sets defined via RLOAD1 or
RLOAD2 cards (for frequency response) or TLOAD1 or TLOAD2 cards (for transient
response).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DLOAD   |  SID  |   S   |  S1   |  L1   |  S2   |  L2   |  S3   |  L3   |+abc |
|+abc    |  S4   |  L4   |       | etc.  |       |       |       |       |     |
|DLOAD   |   17  |  1.0  |  2.0  |   6   | -2.0  |   7   |  2.0  |  8    |+A   |
|+A      | -2.0  |  9    |       |       |       |       |       |       |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

S         Scale factor (Real).

Si        Scale factors (Real).

Li        Load set identification numbers defined via card types enumerated
          above (Integer > 0).

## Remarks

1.The load vector being defined by this card is given by

  {P}  =  S - Si{PLi}
            i

2.The Li must be unique.

3.SID must be different from all Li.

4.Nonlinear transient loads may not be included; they are selected separately
  in the Case Control Deck.

5.Linear load sets must be selected in the Case Control Deck (DLOAD = SID) to
  be used by NASTRAN.

6.A DLOAD card may not reference a set identification number defined by
  another DLOAD card.

7.TLOAD1 and TLOAD2 loads may be combined only through the use of the DLOAD
  card.

8.RLOAD1 and RLOAD2 loads may be combined only through the use of the DLOAD
  card.

9.SID must be unique for all TLOAD1, TLOAD2, RLOAD1, and RLOAD2 cards.


DMI - Direct Matrix Input
=========================

## Description

Used to define matrix data blocks directly. Generates a matrix of the form

          +---                         ---+
          |  A11    A12................A1n|
  [A]  =  |  A21    A22................A2n|
          |   |      |                  | |
          |   |      |                  | |
          |  Am1.......................Amn|
          +---                         ---+

where the elements Aij may be real or complex single-precision or double
precision numbers.

## Format and Example

(The first logical card is a header card.)

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMI     | NAME  |  "0"  | FORM  |  TIN  | TOUT  |       |   M   |   N   |     |
|DMI     |  QQQ  |   0   |   2   |   3   |   3   |       |   4   |   2   |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMI     | NAME  |   J   |   I1  |A(I1,J)|       |       |  etc. |   I2  |+abc |
|+abc    |A(I2,J)|       |  etc. |       |       |       |       |       |     |
|DMI     |  QQQ  |   1   |   1   |  1.0  |  2.0  |  3.0  |  4.0  |   3   |+1   |
|+1      |  5.0  |  6.0  |       |       |       |       |       |       |     |
|DMI     |  QQQ  |   2   |   2   |  6.0  |  7.0  |   4   |  8.0  |  9.0  |     |

                   -etc. for each nonnull column-

Field     Contents
-----     --------
NAME      Any NASTRAN BCD value (1 - 8 alphanumeric characters, the first of
          which must be alphabetic) which will be used in the DMAP sequence
          to reference the data block.

FORM      Matrix form:

          1  Square matrix (not symmetric).
          2  General rectangular matrix.
          6  Symmetric matrix.

TIN       Type of matrix being input as follows:

           1  Real, single-precision (One field is used per element)
           2  Real, double-precision (One field is used per element)
           3  Complex, single-precision (Two fields are used per element)
           4  Complex, double-precision (Two fields are used per element)

TOUT       Type of matrix which will be created:

           1  Real, single-precision    3  Complex, single-precision
           2  Real, double-precision    4  Complex, double-precision

M          Number of rows in A (Integer > 0).

N          Number of columns in A (Integer > 0).

J          Column number of A (Integer > 0).

I1, I2, etc.  Row number of A (Integer > 0).

A(Ix,J)    Element of A (See TIN) (Real).

## Remarks

1. You must write a DMAP (or make alterations to a rigid format) in order to
   use the DMI feature since he is defining a data block. All of the rules
   governing the use of data blocks in DMAP sequences apply. In the example
   shown below, the data block QQQ is defined to be the complex,
   single-precision rectangular 4x2 matrix:

                    +--                      --+
                    |(1.0, 2.0)      (0.0, 0.0)|
   [QQQ]   =        |(3.0, 4.0)      (6.0, 7.0)|
                    |(5.0, 6.0)      (0.0, 0.0)|
                    |(0.0, 0.0)      (8.0, 9.0)|
                    +--                      --+

   The DMAP data block NAME (QQQ in the example) will appear in the initial
   FIAT and the data block will initially appear on the Data Pool File (POOL).

2. A limit to the number of DMIs which may be defined is set by the size of
   the Data Pool Dictionary. The total number of DMIs may not exceed this
   size.

3. There are a number of reserved words which may not be used for DMI names.
   Among  these are POOL, NPTP, OPTP, UMF, NUMF, PLT1, PLT2, INPT, INP1
   through INP9, GEOM1, GEOM2, GEOM3, GEOM4, GEOM5, EDT, MPT, EPT, DIT,
   DYNAMICS, IFPFILE, AXIC, FORCE, MATPOOL, PCDB, XYCDB, CASECC, any DTI
   names, and SCRATCH1 through SCRATCH9.

4. Field 3 of the header card must contain an integer 0.

5. For symmetric matrices, the entire matrix must be input.

6. Only nonzero terms need be entered.

7. A blank field on this card is not equivalent to a zero. If zero input is
   desired, the appropriate type of zero must be punched (that is, 0.0 or
   0.0D0).

8. Complex input must have both the real and imaginary parts punched if either
   part is nonzero.

9. If A (IX,J) is followed by THRU in the next field and an integer row number
   IY after the THRU, then A (IX,J) will be repeated in each row through IY.
   The THRU must follow an element value. In the example below, 3.14 will be
   in rows 3 through 6 of column 1 and 2.0 in row 9.

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMI     |  QQQ  |   0   |   2   |   1   |   1   |       |   9   |   1   |     |
|DMI     |  QQQ  |   1   |   3   |  3.14 | THRU  |   6   |   9   |  2.0  |     |


DMIAX - Direct Axisymmetric Matrix Input
========================================

## Description

Defines axisymmetric (fluid or structure) related direct input matrix terms.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMIAX   | NAME  |  "0"  |  IFO  |  TIN  | TOUT  |       |       |       |     |
|DMIAX   | B2PP  |   0   |   1   |   3   |   4   |       |       |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMIAX   | NAME  |   GJ  |   CJ  |   NJ  |       |       |       |       |+abc |
|+abc    |   GI  |   CI  |   NI  |  Xij  |  Yij  |       |       |       |+def |
|DMIAX   | B2PP  |   32  |       |       |       |       |       |       |+BG27|
|+BG27   | 1027  |   3   |       |4.35+6 |2.27+3 |       |       |       |     |

          -etc. for each column and row containing nonzero terms-

Field      Contents
-----      --------
NAME       BCD name of matrix (one to eight alphanumeric characters the first
           of which is alphabetic).

IFO        Identification of matrix form:

           1  Square matrix
           2  General rectangular matrix
           6  Symmetric matrix

TIN        Type of matrix being input as follows:

           1  Real, single-precision (One field is used per element)
           3  Complex, single-precision (Two fields are used per element)

TOUT       Type of matrix which will be created:

           1  Real, single-precision    3  Complex, single-precision
           2  Real, double precision    4  Complex, double-precision

GJ, GI     Grid, scalar, RINGFL fluid point, PRESPT pressure point, FREEPT
           free surface displacement, or extra point identification number
           (Integer > 0).

CJ, CI     Component number for GJ or GI grid point (0 <= Integer <= 6; Blank
           or zero if GJ or GI is a scalar, fluid, or extra point).

NJ, NI     Harmonic number of RINGFL point. Must be blank if a point type
           other than RINGFL is used. Negative number implies the sine
           series, positive implies the cosine series. (Integer).

Xij, Yij   Real and imaginary parts of matrix element; row (GI, CI, NI)
           column (GJ, CJ, NJ).

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. Matrices defined on this card may be used in dynamics by selection in the
   Case Control Deck by K2PP=NAME, B2PP=NAME, or M2PP=NAME for [K2pp], [B2pp],
   or [M2pp] respectively.

3. In addition to the header card containing IFO, TIN, and TOUT, a logical
   card consisting of two or more physical cards is needed for each nonnull
   column of the matrix.

4. If TIN = 1, Yij must be blank.

5. Field 3 of the header card must contain an integer 0.

6. For symmetric matrices, the entire matrix must be input.

7. Only nonzero terms need be entered.

8. There are a number of reserved words which may not be used for DMIAX names.
   Among these are POOL, NPTP, OPTP, UMF, NUMF, PLT1, PLT2, INPT, GEOM1,
   GEOM2, GEOM3, GEOM4, GEOM5, EDT, MPT, EPT, DIT, DYNAMICS, IFPFILE, AXIC,
   FORCE, MATPOOL, PCDB, XYCDB, CASECC, any DTI names, and SCRATCH1 through
   SCRATCH9.

DMIG - Direct Matrix Input at Grid Points
=========================================

## Description

Defines structure-related direct input matrices.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMIG    | NAME  |  "0"  |  IFO  |  TIN  | TOUT  |       |       |       |     |
|DMIG    | STIF  |   0   |   1   |   3   |   4   |       |       |       |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DMIG    | NAME  |   GJ  |   CJ  |       |   GI  |   CI  |  Xij  |  Yij  |Xabc |
|+abc    |   GI  |   CI  |   Xij |  Yij  |  GI   |  CI   |  Xij  |  Yij  |Xcef |
|DMIG    | STIF  |   27  |   1   |       |   2   |   3   | 3.+5  | 3.+3  |EKG1 |
|+KG1    |   2   |   4   | 2.5+10|   .0  |  50   |       |  1.0  |    0. |     |

              -etc. for each column containing nonzero terms-

Field      Contents
-----      --------
NAME       BCD name of matrix (one to eight alphanumeric characters the first
           of which is alphabetic).

IFO        Identification of matrix form:

           1  Square matrix
           2  General rectangular matrix
           6  Symmetric matrix

TIN        Type of matrix being input as follows:

           1  Real, single-precision (One field is used per element)
           3  Complex, single-precision (Two fields are used per element)

TOUT       Type of matrix which will be created:

           1  Real, single-precision    3  Complex, single-precision
           2  Real, double-precision    4  Complex, double-precision

GJ, GI     Grid or scalar or extra point identification number (Integer > 0).

CJ, CI     Component number for GJ a grid point (0 < CJ <= 6); blank or zero
           for GJ a scalar or extra point.

Xij, Yij   Real and imaginary parts of matrix element.

## Remarks

1. Matrices defined on this card may be used in dynamics by selection in the
   Case Control Deck by K2PP=NAME, B2PP=NAME, or M2PP=NAME for [K2pp], [B2pp],
   or [M2pp], respectively.

2. In addition to the header card containing IFO, TIN, and TOUT, a logical
   card consisting of one or more physical cards is needed for each nonnull
   column of the matrix.

3. If TIN = 1, Yij must be blank.

4. Field 3 of the header card must contain an integer 0.

5. For symmetric matrices, the entire matrix must be input.

6. Only nonzero terms need be entered.

7. The matrix names must be unique among all DMIGs.

8. There are a number of reserved words which may not be used for DMIG names.
   Among these are POOL, NPTP, OPTP, UMF, NUMF, PLT1, PLT2, INPT, GEOM1,
   GEOM2, GEOM3, GEOM4, GEOM5, EDT, MPT, EPT, DIT, DYNAMICS, IFPFILE, AXIC,
   FORCE, MATPOOL, PCDB, XYCDB, CASECC, and DTI names, and SCRATCH1 through
   SCRATCH9.


DPHASE - Dynamic Load Phase Lead
================================

## Description

The DPHASE card is used in conjunction with the RLOAD1 and RLOAD2 data cards
to define the phase lead term é in the equation of the loading function.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DPHASE  |  SID  |   P   |   C   |   TH  |   P   |   C   |   TH  |       |     |
|DPHASE  |   4   |   21  |   6   |  2.1  |   8   |   6   |  7.2  |       |     |

Field      Contents
-----      --------
SID        Identification number of DPHASE set (Integer > 0).

P          Grid or scalar point identification number (Integer > 0).

C          Component number (1 - 6 for grid point, 0 or blank for scalar
           point).

TH         Phase lead é (in degrees) for designated coordinate (Real).

## Remarks

1. One or two dynamic load phase lead terms may be defined on a single card.


DPHASES - Dynamic Load Phase Lead, Substructure Analysis
========================================================

## Description

The DPHASES card is used in conjunction with the RLOAD1 and RLOAD2 data cards
to define the phase lead term é in the equation of the loading function.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DPHASES |  SID  | NAME  |   P   |   C   |   TH  |   P   |   C   |   TH  |     |
|DPHASES |   4   | SKIN  |   21  |   6   |  2.1  |   8   |   6   |  7.2  |     |

Field      Contents
-----      --------
SID        Identification number of DPHASE set (Integer > 0).

NAME       Basic substructure name.

P          Grid or scalar point identification number (Integer > 0).

C          Component number (1 - 6 for grid point, 0 or blank for scalar
           point).

TH         Phase lead é (in degrees) for designated coordinate (Real).

## Remarks

1. One or two dynamic load phase lead terms may be defined on a single card.

2. Used in substructure SOLVE operation.

3. Points referenced must exist in the SOLVEd structure.

DSFACT - Differential Stiffness Factor
======================================

## Description

Used to define a scale factor for applied loads and stiffness matrix in a
Normal Modes with Differential Stiffness Analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DSFACT  |  SID  |   B   |       |       |       |       |       |       |     |
|DSFACT  |   97  |  -1.0 |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (unique Integer > 0).

B          Scale factor (Real).

## Remarks

1. Load sets must be selected in the Case Control Deck (DSCO = SID) to be used
   by NASTRAN.

2. All fields following the entry must be blank.

DTI - Direct Table Input

## Description

Used to define table data blocks directly.

## Format and Example

The first logical card is a header card.

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DTI     |  NAME |  "0"  |   T1  |   T2  |   T3  |   T4  |   T5  |   T6  |+00  |
|+00     |   V   |   V   |       | etc.  |ENDREC |       |       |       |+01  |
|DTI     |  XXX  |   0   |   3   |   4   |  4096 | 32768 |   1   |   0   |     |

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|DTI     |  NAME |  IREC |   V   |   V   |   V   |   V   |   V   |   V   |+11  |
|+11     |   V   |   V   |   V   |   V   |  etc. |       |ENDREC |       |+12  |
|DTI     |  XXX  |   1   |  2.0  |  -6   |  ABC  | 6.0D0 |  -1   |   2   |+11  |
|+11     |   4   | -6.2  |  2.9  |   1   |  DEF  |  -1   |ENDREC |       |     |

Field      Contents
-----      --------
NAME       Any NASTRAN BCD value (1 to 8 alphanumeric characters, the first
           of which must be alphabetic) which will be used in the DMAP
           sequence to reference the data block.

Ti         Trailer values (65535 >= Integer >= 0).

IREC       Record number (sequential integer beginning with 1).

V          Value (blank, integer, real, BCD (except ENDREC), double
           precision).

ENDREC     The BCD value ENDREC which flags the end of the string of values
           that constitute logical record IREC.

## Remarks

1. Records may be made as long as desired via continuation cards.

2. Values may be of any type (blank, integer, real, BCD, double precision)
   with the exception that a BCD value may not be ENDREC.

3. All fields following ENDREC must be blank.

4. You must write a DMAP (or make alterations to a rigid format) in order to
   use the DTI feature since he is defining a data block. All of the rules
   governing the use of data blocks in DMAP sequences apply.

5. The DMAP data block NAME (XXX in the example) will appear in the initial
   FIAT and the data block will initially appear on the POOL.

6. If trailer is not specified, T1 = number of records, T2 through T6 = 0.

7. In addition to the header card, there must be one logical card for each
   record in the table.

8. There are a number of reserved words which may not be used for DTI names.
   Among these are POOL, NPTP, OPTP, UMF, NUMF, PLT1, PLT2, INPT, GEOM1,
   GEOM2, GEOM3, GEOM4, GEOM5, EDT, MPT, EPT, DIT, DYNAMICS, IFPFILE, AXIC,
   FORCE, MATPOOL, PCDB, XYCDB, CASECC, any DTI names, and SCRATCH1 through
   SCRATCH9.


EIGB - Buckling Analysis Data
=============================

## Description

Defines data needed to perform buckling analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EIGB    |  SID  |METHOD |   L1  |   L2  |  NEP  |  NDP  |  NDN  |   E   |+abc |
|+abc    | NORM  |   G   |   C   |       |       |       |       |       |     |
|EIGB    |   13  | DET   |  0.1  |  2.5  |   2   |   1   |   1   |  0.0  |ABC  |
|+BC     |  MAX  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (unique Integer > 0).

METHOD     Method of eigenvalue extraction, one of the BCD values INV, DET,
           FEER, UINV, or UDET.

           INV   Inverse power method, symmetric matrix operations
           DET   Determinant method, symmetric matrix operations
           FEER  Tridiagonal reduction method, symmetric matrix operations
           UINV  Inverse power method, unsymmetric matrix operations
           UDET  Determinant method, unsymmetric matrix operations

L1, L2     Eigenvalue range of interest (Real; L1 < L2 > 0.0). For METHOD =
           FEER, L1 is ignored and L2 is the acceptable relative error
           tolerance on eigenvalues, (default is .1/n where n is the order of
           the stiffness matrix.) (Real > 0.0).

NEP        Estimate of number of roots in positive range. Desired number of
           eigenvalues of smallest magnitude for METHOD = FEER. (Default is
           automatically calculated to extract at least one accurate mode.)
           (Integer > 0).

NDP, NDN   Desired number of positive and negative roots (default = 3 x NEP)
           (Integer > 0) Ignored for METHOD = FEER.

E          Convergence criteria (optional) (Real > 0.0).

NORM       Method for normalizing eigenvectors, one of the BCD values MAX or
           POINT.

           MAX   Normalize to unit value of the largest component in the
                 analysis set.

           POINT Normalize to unit value of the component defined in fields 3
                 and 4 (defaults to MAX if defined component is zero).

G          Grid or scalar point identification number (Integer > 0) (required
           if and only if NORM = POINT).

C          Component number (one of the integers 1 - 6) (required if and only
           if NORM = POINT and G is a geometric grid point.)

## Remarks

1. Buckling analysis root extraction data sets must be selected in the Case
   Control Deck (METHOD = SID) to be used by NASTRAN.

2. The quantities L1 and L2 are dimensionless and specify a range in which the
   eigenvalues are to be found. An eigenvalue is a factor by which the
   pre-buckling state of stress (first subcase) is multiplied to produce
   buckling. If METHOD = FEER, L1 is ignored and L2 represents the maximum
   upper bound, in percent, on | lambdaFEER / lambdaEXACT - 1 | for acceptance
   of a computed eigensolution.

3. The continuation card is required.

4. See Sections 10.3.6 and 10.4.2.2 of the Theoretical Manual for a discussion
   of convergence criteria.

5. If METHOD = DET, L1 must be greater than or equal to 0.0.

6. If NORM = MAX, components that are not in the analysis set may have values
   larger than unity.

7. If NORM = POINT, the selected component must be in the analysis set.


EIGC - Complex Eigenvalue Extraction Data
=========================================

## Description

Defines data needed to perform complex eigenvalue analysis.

                                          /
                               w          l2
                               |             /
                               |  a1   /\ b2
                               |+-+-+ / /\
                               || | |/ / /
                               || | | / /
                               || |/|/ /
                               || | | /
                               ||/|/|/
                               || | |
                               ||/|/|
                              /|| | |
                             / +------------------------- +
                            / / | | |
                            \/ /+-+-+
                           a2\/   b1

                                | l1|

                          Figure 2.4-42. EIGC diagram

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EIGC    |  SID  |METHOD |  NORM |   G   |   C   |   E   |       |       |+abc |
|+abc    | +a1   | wa1   | +b1   | wb1   | l1    | Ne1   | Nd1   |       |+def |
|+def    | +a2   | wa2   | +b2   | wb2   | l2    | Ne2   | Nd2   |       |     |
|EIGC    |   14  | DET   |  POINT|   27  |       | 1.-8  |       |       |ABC  |
|+BC     | 2.0   | 5.6   | 2.0   | -3.4  | 2.0   |  4    |  4    |       |DEF  |
|+EF     | -5.5  | -5.5  |  5.6  |  5.6  |  1.5  |  6    |  3    |       |     |

Field      Contents
-----      --------
SID        Set identification number (unique Integer > 0).

METHOD     Method of complex eigenvalue extraction, one of the BCD values
           INV, DET, HESS, or FEER.

           INV   Inverse power method
           DET   Determinant method
           HESS  Upper Hessenberg method
           FEER  Tridiagonal Reduction method

NORM       Method for normalizing eigenvectors, one of the BCD values MAX or
           POINT

           MAX   Normalize to a unit value for the real part and a zero value
                 for the imaginary part, the component having the largest
                 magnitude.

           POINT Normalize to a unit value for the real part and a zero value
                 for the imaginary part the component defined in fields 5 and
                 6 - defaults to MAX if the magnitude of the defined
                 component is zero.

G          Grid or scalar point identification number (required if and only
           if NORM=POINT) (Integer > 0).

C          Component number (required if and only if NORM = POINT and G is a
           geometric grid point) (0 <= Integer <= 6).

E          Convergence criterion (optional) (Real >= 0.0) For METHOD = FEER,
           error-tolerance on acceptable eigenvalues (default value is .10/n,
           where n is the order of the stiffness matrix).

(+aj, waj),(+bj, wbj)  Two complex points defining a line in the complex plane
           (Real) For METHOD = FEER, (+aj, waj) is a point of interest in the
           complex plane, closest to which the eigenvalues are computed;
           |+aj| + |waj| > 0. The point (+bj, wbj) is ignored.

lj         Width of region in complex plane  (Real > 0.0) Blank for METHOD =
           FEER.

Nej        Estimated number of roots in each region (Integer > 0). Ignored
           for METHOD = FEER.

Ndj        Desired number of roots in each region (default is 3Nej) (Integer
           > 0) Desired number of accurate roots for METHOD = FEER (default
           is 1).

## Remarks

1. Each continuation card defines a rectangular search region. For METHOD =
   FEER, the card defines a circular search region, centered at (+aj, waj) and
   of sufficient radius to encompass Ndj roots. Any number of regions may be
   used and they may overlap. Roots in overlapping regions will not be
   extracted more than once.

2. Complex eigenvalue extraction data sets must be selected in the Case
   Control Deck (CMETHOD = SID) to be used by NASTRAN.

3. The units of +, w, and l are radians per unit time.

4. At least one continuation card is required.

5. For the determinant method with no damping matrix, complex conjugates of
   the roots found are not printed.

6. See Section 10.4.4.5 of the Theoretical Manual for a discussion of
   convergence criteria.

7. For the Upper Hessenberg method, Ndl controls the number of eigenvectors
   computed. Only one continuation card is considered and the (+,w) pairs,
   along with the parameters l1 and Ne1, are ignored. Insufficient storage for
   HESS will cause the program to switch to INV.

8. The error tolerance, E, for the FEER method is with regard to

   |   _                     |
   | | pi - (+aj, waj)|      |
   |-------------------  - 1 |  for [B] not equal [0] and
   | | pi - (+aj, waj)|      |
   |                         |

   |   _2             2      |
   | | pi - (+aj, waj) |     |
   |-------------------- - 1 |  for [B] = [0],
   |    2             2      |
   | | p  - (+aj, waj) |     |
   |                         |

   where i is a computed eigenvalue and pi an exact eigenvalue.

9. The complex eigenvalue is given by + + iw = 2+f (i - 1/2 g), where f is the
   frequency and g is the damping coefficient.

10.   The default of NORM is MAX.


EIGP - Poles in Complex Plane
=============================

## Description

Defines poles that are used in complex eigenvalue extraction.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EIGP    |  SID  |   +   |   w   |   M   |   +   |   w   |   M   |       |     |
|EIGP    |   15  | -5.2  |  0.0  |   2   |  6.3  |  5.5  |   3   |       |     |

Field     Contents
-----     --------
SID       Set identification number (Integer > 0).

(+,w)     Coordinates of point in complex plane (Real).

M         Multiplicity of complex root at pole defined by (+,w) (Integer >
          0).

## Remarks

1.Defines poles in complex plane that are used with associated EIGC card
  having same set number.

2.The units of +,w are radians per unit time.

3.Poles are used only in the determinant method.

4.One or two poles may be defined on a single card.


EIGR - Real Eigenvalue Extraction Data
======================================

## Description

Defines data needed to perform real eigenvalue analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EIGR    |  SID  |METHOD |   F1  |   F2  |  NE   |  ND   |  NZ   |   E   |+abc |
|+abc    | NORM  |   G   |   C   |       |       |       |       |       |     |
|EIGR    |   13  | DET   |  1.9  | 15.6  |  10   |  12   |   0   |  1.-3 |ABC  |
|+BC     | POINT |   32  |   4   |       |       |       |       |       |     |

Field     Contents
-----     --------
SID       Set identification number (unique Integer > 0).

METHOD    Method of eigenvalue extraction, one of the BCD values INV, DET,
          GIV, MGIV, FEER, FEER-Q, FEER-X, UINV, or UDET.

          INV     Inverse power method, symmetric matrix operations.

          DET     Determinant method, symmetric matrix operations.

          GIV     Givens method of tridiagonalization.

          MGIV    Modified Givens method (see Remark 11).

          FEER    Tridiagonal reduction method, symmetric matrix
                  operations.

          FEER-Q  See Remark 12.

          FEER-X  See Remark 12.

          UINV    Inverse power method, unsymmetric matrix operations.

          UDET    Determinant method, unsymmetric matrix operations.

F1, F2    Frequency range of interest (Required for METHOD = DET, INV, UDET,
          or UINV) (Real >= 0.0; F1 <= F2); If METHOD = GIV, frequency range
          over which eigenvectors are desired. The frequency range is
          ignored if ND > 0, in which case the eigenvectors for the first ND
          positive roots are found. (Real, F1 <= F2). If METHOD = FEER, F1
          is the center of range of interest (Default is F1 = 0.0) (Real >=
          0.0), and F2 is the acceptable relative error tolerance, as a
          percentage, on frequency-squared (Default, as a percentage, is
          0.1/n where n is the order of the stiffness matrix)  (Real > 0.0).

NE        Estimate of number of roots in range (Required for METHOD = DET,
          INV, UDET, or UINV, ignored for METHOD = FEER) (Integer > 0).

NE (GIVENS)Number of roots to be printed (default all) (rigid roots
          included).

ND        Desired number of roots for METHOD = DET, INV, UDET, or UNIV,
          (Default is 3 NE) (Integer > 0). Desired number of eigenvectors
          for METHOD = GIV (Integer > 0). Desired number of roots and
          eigenvectors for METHOD = FEER (Default is automatically
          calculated to extract at least one accurate mode) (Integer > 0).

NZ        Number of free body modes (Optional; used only if METHOD = DET or
          UDET) (Integer> 0).

E         Mass orthogonality test parameter (Default is 0.0 which means no
          test will be made)  (Real   0.0).

NORM      Method for normalizing eigenvectors, one of the BCD values MASS,
          MAX, or POINT.

          MASS    Normalize to unit value of the generalized mass.

          MAX     Normalize to unit value of the largest component in the
                  analysis set.

          POINT   Normalize to unit value of the component defined in
                  fields 3 and 4 - defaults to MAX if defined component is
                  zero.

G         Grid or scalar point identification number (Required if and only
          if NORM=POINT) (Integer >= 0).

C         Component number (One of the integers 1 - 6) (Required if and only
          if NORM=POINT and G is a geometric grid point).

## Remarks

1.Real eigenvalue extraction data sets must be selected in the Case Control
  Deck (METHOD = SID) to be used by NASTRAN.

2.The units of F1 and F2 are cycles per unit time. If METHOD = FEER, F2
  represents the maximum upper bound, in percent, on

    2       2
  |w     / w      - 1|
    FEER    EXACT

  for acceptance of a computed eigensolution.

3.The continuation card is required.

4.If METHOD = GIV, all eigenvalues are found.

5.If METHOD = GIV, the mass matrix for the analysis set must be positive
  definite.This means that all degrees of freedom, including rotations, must
  have mass properties. OMIT cards may be used to remove massless degrees of
  freedom.

6.A nonzero value of E in field 9 also modifies the convergence criteria. See
  Sections 10.3.6 and 10.4.2.2 of the Theoretical Manual for a discussion of
  convergence criteria.

7.If NORM = MAX, components that are not in the analysis set may have values
  larger than unity.

8.If NORM = POINT, the selected component must be in the analysis set.

9.If METHOD = GIV and rigid body modes are present, F1 should be set to zero
  if the rigid body eigenvectors are desired.

10.  The desired number of roots (ND) includes all roots previously found,
     such as rigid body modes determined with the use of the SUPORT card, or
     the number of roots previously checkpointed when restarting and
     APPENDing the eigenvector file. The APPEND feature is available in the
     case of the Determinant, Inverse Power and FEER methods of eigenvalue
     extraction.

11.  Givens method requires the mass matrix not to be singular. The MGIV
     method allows the mass matrix to be singular. However, the dynamic
     matrices could be bigger, or much bigger, which would require more CPU
     time and core space.

12.  The rigid body frequencies are zero substituted unless FEER-X is
     requested. If FEER-Q is requested, certain key areas in FEER
     computations are done in quad precision (Real*16) for 32-bit word
     machines and in double precision for 60- and 64-bit word machines. The
     FEER-Q request would yield much better rigid body eigenvalues, but it
     may take two to three times longer to compute than FEER or FEER-X.


ENDDATA - End of Bulk Data
==========================

## Description

Defines the end of the Bulk Data Deck.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|ENDDATA |       |       |       |       |       |       |       |       |     |

First Alternate Form:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|ENDATA  |       |       |       |       |       |       |       |       |     |

Second Alternate Form:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|END DATA|       |       |       |       |       |       |       |       |     |

## Remarks

1.This card is required even if no physical data cards exist in the deck.

2.ENDDATA may begin in column 1 or 2. If the first alternate form is used,
  ENDATA may begin in column 1, 2, or 3. If the second alternate form is
  used, END DATA must necessarily begin in column 1.

3.Failure to include this card will result in job termination caused by an
  end-of-file condition being encountered on the input file.

4.Extraneous data cards may be stored after this card except when the INPUT
  module data follows or when the UMF card FINIS follows or when multiple job
  steps occur within the same job submittal on the CDC computer.


EPOINT - Extra Point
====================

## Description

Defines extra points of the structural model for use in dynamics problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EPOINT  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |     |
|EPOINT  |   3   |   18  |   1   |   4   |   16  |   2   |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|EPOINT  |  ID1  | "THRU"|  ID2  |       |       |       |       |       |     |
|EPOINT  |  17   |  THRU |   43  |       |       |       |       |       |     |

Field     Contents
-----     --------
ID, ID1, ID2  Extra point identification number (Integer > 0; ID1 < ID2).

## Remarks

1.All extra point identification numbers must be unique with respect to all
  other structural, scalar, and fluid points.

2.This card is used to define coordinates used in transfer function
  definitions (see TF card).

3.If the alternate form is used, extra points ID1 through ID2 are defined.


FLFACT - Aerodynamic Physical Data
==================================

## Description

Used to specify densities, Mach numbers, or interblade phase angles, and
reduced frequencies for flutter analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FLFACT  |  SID  |   F1  |   F2  |   F3  |   F4  |   F5  |   F6  |   F7  |ABC  |
|+BC     |   F8  |   F9  | etc.  |       |       |       |       |       |     |
|FLFACT  |   97  |   .3  |   .7  |   3.5 |       |       |       |       |abc  |

## Alternate Form


|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FLFACT  |  SID  |   F1  |  THRU |  FNF  |   NF  |  FMID |       |       |     |
|FLFACT  |  201  |  .200 |  THRU |  .100 |   11  |.133333|       |       |     |

Field     Contents
-----     --------
SID       Set identification number (unique Integer > 0).

Fi        Aerodynamic factor (Real).

## Remarks

1.These factors must be selected by a FLUTTER data card to be used by
  NASTRAN.

2.Imbedded blank fields are forbidden.

3.Parameters must be listed in the order in which they are to be used within
  the looping of flutter analysis.

4.For the alternate form, NF must be greater than 1. Fmid must lie between F1
  and FNF, otherwise Fmid will be set to (F1 + FNF)/2. Then

        F1(FNF - Fmid)(NF - i) + FNF(Fmid - F1)(i - 1)
  Fi =  ----------------------------------------------      i=1,2,...,NF
          (FNF - Fmid)(NF - i) + (Fmid - F1)(i - 1)

  The use of Fmid (middle factor selection) allows unequal spacing of the
  factors. Fmid = 2F1FNF/(F1+FNF) gives equal values to increments of the
  reciprocal of F1.


FLSYM - Axisymmetric Symmetry Control
=====================================

## Description

Defines the relationship between the axisymmetric fluid and a structural
boundary having symmetric constraints. The purpose is to allow fluid boundary
matrices to conform to structural symmetry definitions.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FLSYM   |   M   |   S1  |   S2  |       |       |       |       |       |     |
|FLSYM   |   12  |   S   |   A   |       |       |       |       |       |     |

Field     Contents
-----     --------
M         Number of symmetric sections of structural boundary around
          circumference of fluid being modeled by the set of structural
          elements (Integer >= 2, even).

S1, S2    Description of boundary constraints used on structure at first and
          second planes of symmetry. (BCD: S for symmetric, A for
          antisymmetric).

## Remarks

1.This card is allowed only if an AXIF card is also present.

2.Only one (1) FLSYM card is allowed.

3.The card is not required if no planes of symmetry are involved.

4.First plane of symmetry is assumed to be at í = 0. Second plane of symmetry
  is assumed to be at í = 360 degrees/M.

5.Symmetric and antisymmetric constraints for the structure must, in
  addition, be provided by you.

6.The solution is performed for those harmonic indices listed on the AXIF
  card that are compatible with the symmetry conditions.

Example

If a quarter section of structure is used to model the boundary, M = 4. If the
boundary constraints are S-S, the compatible cosine harmonics are:  0, 2, 4,
etc. If S-A is used the compatible cosine harmonics are 1, 3, 5, ..., etc.


FLUTTER - Aerodynamic Flutter Data
==================================

## Description

Defines data needed to perform flutter analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FLUTTER |  SID  |METHOD | DENS  |  MACH | RFREQ | IMETH |NVALUE |  EPS  |     |
|FLUTTER |   19  |  K    |  119  |   219 |  319  |   S   |   5   |  1.-4 |     |

Field     Contents
-----     --------
SID       Set identification number (unique Integer > 0).

METHOD    Flutter analysis method, K for K method, PK for P-K method, KE for
          the K method restricted for efficiency.

DENS        Identification number of an FLFACT data card specifying density
            ratios to be used in flutter analysis (Integer >= 0).

MACH        Identification number of an FLFACT data card specifying Mach
            numbers or interblade phase angles (m) to be used in flutter
            analysis (Integer >= 0).

RFREQ (or VEL)  Identification number of an FLFACT data card specifying
            reduced frequencies (k) to be used in flutter analysis (Integer >
            0); for the P-K method, the velocity.

IMETH       Choice of interpolation method for matrix interpolation (BCD:  L
            for linear,  S for surface).

NVALUE      Number of eigenvalues for output and plots (Integer > 0).

EPS         Convergence parameter for k; used in the P-K method (Real)
            (default = 10**(-3)).

## Remarks

1. The FLUTTER data card must be selected in Case Control Deck (FMETHOD =
   SID).

2. The density is given by DENS * RHOREF, where RHOREF is the reference value
   given on the AERO data card.

3. The reduced frequency is given by k = (REFC*w/2*V), where REFC is given on
   the AERO data card, w is the circular frequency, and V is the velocity.

4. An eigenvalue is accepted in the P-K method when | k - kestimate | < EPS.


FORCE - Static Load
===================

## Description

Defines a static load at a grid point by specifying a vector.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FORCE   |  SID  |   G   |  CID  |   F   |   N1  |   N2  |   N3  |       |     |
|FORCE   |   2   |   5   |   6   |  2.9  |  0.0  |  1.0  |  0.0  |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

CID         Coordinate system identification number (Integer >= 0).

F           Scale factor (Real).

N1, N2, N3  Components of vector measured in coordinate system defined by CID
            (Real; N1**2 + N2**2 + N3**2 > 0.0).

## Remarks

1. The static load applied to grid point G is given by

   ->      ->
   f  =  F N

   where N is the vector defined in fields 6, 7, and 8.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

3. A CID of zero references the basic coordinate system.

FORCE1 - Static Load

## Description

Used to define a static load by specification of a value and two grid points
which determine the direction.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FORCE1  |  SID  |   G   |   F   |   G1  |   G2  |       |       |       |     |
|FORCE1  |   6   |   13  | -2.93 |   16  |   13  |       |       |       |     |

Field       Contents.

SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

F           Value of load (Real).

G1, G2      Grid point identification numbers (Integer > 0; G1 not equal G2).

## Remarks

1. The direction of the force is determined by the vector from G1 to G2.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.


FORCE2 - Static Load
====================

## Description

Used to define a static load by specification of a value and four grid points
which determine the direction.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FORCE2  |  SID  |   G   |   F   |   G1  |   G2  |   G3  |   G4  |       |     |
|FORCE2  |   6   |   13  | -2.93 |   16  |   13  |   17  |   13  |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

F           Value of load (Real).

G1,...,G4   Grid point identification numbers (Integer > 0; G1 through G4 must
            be unique).

## Remarks

1. The direction of the force is determined by the vector product whose
   factors are vectors from G1 to G2 and G3 to G4 respectively.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.


FORCEAX - Axisymmetric Static Load
==================================

## Description

Defines a static loading for a model containing CCONEAX, CTRAPAX, or CTRIAAX
elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FORCEAX |  SID  |  RID  |  HID  |   S   |   FR  |   FP  |   FZ  |       |     |
|FORCEAX |   1   |   2   |   3   |  2.0  |  0.1  |  0.2  |  0.3  |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

RID         Ring identification number (see RINGAX) (Integer > 0).

HID         Harmonic identification number (Integer >= 0 or a sequence of
            harmonics; see Remark 4).

S           Scale factor for load (Real).

FR, FP, FZ  Load components in r, í, z directions (Real).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. Axisymmetric loads must be selected in the Case Control Deck (LOAD = SID)
   to be used by NASTRAN.

3. A separate card is needed for the definition of the force associated with
   each harmonic.

4. If a sequence of harmonics is to be placed in HID the form is as follows:
   "Sn1Tn2" where n1 is the start of the sequence and n2 is the end of the
   sequence; that is, harmonics 0 through 10, the field would contain "S0T10".

5. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

6. For a discussion of the axisymmetric solid problem see Section 5.11 of the
   Theoretical Manual.


FREEPT - Fluid Free Surface Point
=================================

## Description

Defines the location of points on the surface of a fluid for recovery of
surface displacements in a gravity field.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FREEPT  |  IDF  |       |  IDP  |  í   |  IDP  |  í   |  IDP  |  í   |     |
|FREEPT  |   3   |       |  301  | 22.5  |  302  | 90.0  |  303  | 370.0 |     |

Field       Contents
-----       --------
IDF         Fluid point (RINGFL) identification number (Integer > 0).

IDP         Free surface point identification number (Integer > 0).

í           Azimuthal position of FREEPT on fluid point (RINGFL), in fluid
            coordinate system (Real).

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. All free surface point identification numbers must be unique with respect
   to other scalar, structural, and fluid points.

3. The free surface points are used for the identification of output data
   only.

4. Three points may be defined on a single card.

5. The referenced fluid point (IDF) must be included in a free surface list
   (FSLIST).

6. Output requests for velocity and acceleration can be made at these points.


FREQ - Frequency List
=====================

## Description

Defines a set of frequencies to be used in the solution of frequency response
problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FREQ    |  SID  |   F   |   F   |   F   |   F   |   F   |   F   |   F   |abc  |
|+bc     |   F   |   F   |   F   |   F   |   F   |   F   |   F   |   F   |     |
|FREQ    |   3   | 2.98  | 3.05  | 17.9  | 21.3  | 25.6  | 28.8  | 31.2  |ABC  |
|+BC     | 29.2  | 22.4  | 19.3  |       |       |       |       |       |     |

Field       Contents

SID         Frequency set identification number (Integer > 0).

F           Frequency value (Real > 0.0).

## Remarks

1. The units for the frequencies are cycles per unit time.

2. Frequency sets must be selected in the Case Control Deck (FREQ = SID) to be
   used by NASTRAN.

3. All FREQ, FREQ1, and FREQ2 cards must have unique frequency set
   identification numbers.


FREQ1 - Frequency List
======================

## Description

Defines a set of frequencies to be used in the solution of frequency response
problems by specification of a starting frequency, frequency increment, and
number of increments desired.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FREQ1   |  SID  |   F1  |   DF  |  NDF  |       |       |       |       |     |
|FREQ1   |   6   |  2.9  |  0.5  |   13  |       |       |       |       |     |

Field       Contents

SID         Frequency set identification number (Integer > 0).

F1          First frequency in set (Real >= 0.0).

DF          Frequency increment (Real > 0.0).

NDF         Number of frequency increments (Integer > 0).

## Remarks

1. The units for the frequency F1 and the frequency increment DF are cycles
   per unit time.

2. The frequencies defined by this card are given by

   f   =  F1 + (i  - 1) DF,  i  =  1, NDF + 1
    i

3. Frequency sets must be selected in the Case Control Deck (FREQ = SID) to be
   used by NASTRAN.

4. All FREQ, FREQ1, and FREQ2 cards must have unique frequency set
   identification numbers.


FREQ2 - Frequency List
======================

## Description

Defines a set of frequencies to be used in the solution of frequency response
problems by specification of a starting frequency, final frequency, and number
of logarithmic increments desired.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FREQ2   |  SID  |   F1  |   F2  |   NF  |       |       |       |       |     |
|FREQ2   |   6   |  1.0  |  1.E5 |   5   |       |       |       |       |     |

Field       Contents

SID         Frequency set identification number (Integer > 0).

F1          First frequency (Real > 0.0).

F2          Last frequency (Real > 0.0; F2 > F1).

NF          Number of logarithmic intervals (Integer > 0).

## Remarks

1. The units for the frequencies F1 and F2 are cycles per unit time.

2. The frequencies defined by this card are given by

   f   =  F1*e**(i-1)d ,    i = 1,2,...,NF + 1
    i

   where

           1         F2
   d =    ---- log   ---
           NF     e  F1

   For the example shown, the list of frequencies will be 1.0, 10.0, 100.0,
   1000.0,10000.0, and 100000.0 cycles per unit time.

3. Frequency sets must be selected in the Case Control Deck (FREQ = SID) to be
   used by NASTRAN.

4. All FREQ, FREQ1, and FREQ2 cards must have unique frequency set
   identification numbers.


FSLIST - Free Surface List
==========================

## Description

Declares the fluid points (RINGFL) which lie on a free surface boundary.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|FSLIST  |  RHO  |  IDF1 |  IDF2 |  IDF3 |  IDF4 |  IDF5 |  IDF6 |  IDF7 |abc  |
|+bc     |  IDF8 |  IDF9 |  etc. |       |       |       |       |       |def  |
|FSLIST  | 1.0-4 |   1   |   3   |   5   |   4   |   2   |   7   |   6   |+12FS|
|+12FS   |   8   |   9   |  10   |  11   | AXIS  |       |       |       |     |

Field       Contents

RHO         Mass density at the surface (Real > 0.0 or blank; if blank the
            AXIF default value must not be blank).

IDFi        Identification number of RINGFL point (Integer > 0 or BCD "AXIS".
            The first and/or last entry may be AXIS).

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. Each logical card defines a surface. The order of the points must be
   sequential with the fluid on the right with respect to the direction of
   travel.

3. The BCD word AXIS defines an intersection with the polar axis of the fluid
   coordinate system.

4. There may be as many FSLIST cards as required. If the fluid density varies
   along the boundary there must be one FSLIST card for each interval between
   fluid points.


GEMLOOP - General Current Loop
==============================

## Description

Defines a general current loop in magnetic field problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GEMLOOP |  SID  |   I   |  CID  |  X1   |  Y1   |  Z1   |  X2   |  Y2   |+a   |
|+a      |  Z2   |  X3   |  Y3   |  Z3   |       |       |       |       |+b   |
|GEMLOOP |   5   |  5.2  |   0   |  8.1  | 10.2  |  3.5  | 12.5  |  9.1  |+A   |
|+A      |  1.3  | ENDT  |       |       |       |       |       |       |     |

Field       Contents

SID         Load set identification number (Integer > 0).

I           Current through loop (Real > 0.0).

CID         Coordinate system identification number (Integer > 0 or blank).

Xi, Yi, Zi  Coordinates of points defining linear sections of coil in
            coordinate system CID (Real).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. In order for the coil to be closed, XN, YN, ZN must be equal to X1, Y1, Z1.

3. ENDT must be specified in the field immediately after ZN.

4. N should be such that 2 <= N <= 15.

5. If a loop has more than 14 segments, another GEMLOOP card may be specified
   with the first point coincident with the last point of the previous card.

6. CID must presently be 0 or blank.


# GENEL - General Element

## Description

Defines a general element using either of two approaches as follows.

   1. The stiffness approach:

    f      +               +  u
     i     |    K  |   -KS |    i
   ---  =  | ------+------ | ----  ,  or
    f      |   T   |   T   |  u
     d     | -S K     S KS |    d
           +               +

   2. The flexibility approach:

    u      +               +  f
     i     |    Z  |   S   |    i
   ---  =  | ------+-----  |  ---  ,  where
    f      |    T  |       |  u
     d     |  -S       0   |    d
           +               +

                         T
          {u } = [u  ,u  ,...,u  ]  ,
   i      i1  i2      im
   
                         T
          {u } = [u  ,u  ,...,u  ]  ,
   d      d1  d2      dn
                       +-                       -+
                       | KZ      KZ    . . . KZ  |
                       |   11      12          1m|
                       |  .      KZ    . . . .   |
                       |  .        22         .  |
                       |  .       .           .  |            T
   [KZ] = [K] or [Z] = |  .       .           .  |    and [KZ]  = [KZ]  ,
                       | KZ   . . . . . . . KZ   |
                       |   m1                 mn |
                       +-                       -+

          +-                      -+
          | S    . . . . . . .  S  |
          |  11                  1n|
   [S] =  | .                   .  |
          | .                   .  |
          | S    . . . . . . .  S  |
          |  m1                  mn|
          +-                       +

The required input is the {ui} list and the lower triangular portion of [K] or
[Z]. Additional input may include the {ud} list and [S]. If [S] is input, {ud}
must also be input. If {ud} is input but [S] is omitted, [S] is internally
calculated. In this case, {ud} must have six and only six degrees of freedom.
If [S] is not required, both {ud} and [S] are omitted.

Format

(An example is given following.)

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GENEL   |  EID  |       |  UI1  |  CI1  |  UI2  |  CI2  |  UI3  |  CI3  | X1  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +1    |  UI4  |  CI4  |  UI5  |  CI5  |  UI6  |  CI6  |  UI7  |  CI7  | X2  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +2    |       |       |       |     etc.      |       |       |       | X3  |
+--------+-------+-------+-------+---------------+-------+-------+-------+-----+
|  +3    |  UI   -    The last item in the UI-list will appear in        | X4  |
|        |    m       one of fields 2, 4, 6, or 8                        |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +4    | "UD"  |       |  UD1  |  CD1  |  UD2  |  CD2  |  UD3  |  CD3  | X5  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +5    |       |       |       |     etc.      |       |       |       | X6  |
+--------+-------+-------+-------+---------------+-------+-------+-------+-----+
|  +6    |  UD   -    The last item in the UD list will appear in        | X7  |
|        |    n       one of fields 2, 4, 6, or 8                        |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +7    |"K"or"Z"  KZ11 |  KZ21 | KZ31  |  etc. |       | KZ22  | KZ32  | X8  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +8    |  etc. |       |  KZ33 | KZ43  |  etc. |       |       |       | X9  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +9    |       |       |       |     etc.      |       |       |       | X10 |
+--------+-------+-------+-------+---------------+-------+-------+-------+-----+
|  +10   |  KZ   -    The last item in the K or Z matrix, will appear in |     |
|        |    mm      one of fields 2 through 9.                         | X11 |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +11   |  "S"  |  S11  |  S12  |  etc. |       |  S21  |  etc. |       | X12 |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|  +12   |   S   -    The last item in the S matrix, will appear in      |     |
|        |    mn      one of fields 2 through 9.                         |     |
+--------+---------------------------------------------------------------+-----+

Field       Contents
-----       --------
EID         Unique element identification number, a positive integer.

UI1, CI1 etc.; UD1, ED1, etc.  Identification numbers of coordinates in the UI
            or UD list, in sequence corresponding to the [K], [Z], and [S]
            matrices. Ui and UDi are grid point numbers, and CIi and CDi are
            the component numbers. If a scalar point is given, the component
            number is zero.

KZij        Values of the [K] or [Z] matrix ordered by columns from the
            diagonal, according to the UI list.

Sij         Values of the [S] matrix ordered by rows, according to the UD
            list.

UD, K, Z, S BCD data words which indicate the start of data belonging to UD,
            [K], [Z], or [S].

## Remarks

1. When the stiffness matrix, K, is input, the number of significant digits
   should be the same for all terms.

2. Double-field format may be used for input of K or Z.

Example
-------
Let element 629 be defined by

                               T
   {u }  =  [1-1 ,13-4,42,24-2]    ,
     i

                    T
   {u }  =  [6-2,33]    ,
     d

where i-j means the jth component of grid point i. Points 42 and 33 are scalar
points.

         +-                     -+                   +-          -+
         | 1.0   2.0   3.0   4.0 |                   | 1.5    2.5 |
         |                       |                   |            |
         | 2.0   5.0   6.0   7.0 |                   | 3.5    4.5 |
   [K] = |                       |   ,         [S]  =|            |
         | 3.0   6.0   8.0   9.0 |                   | 5.5    6.5 |
         |                       |                   |            |
         | 4.0   7.0   9.0   0.0 |                   | 7.5    8.5 |
         +-                     -+                   +-          -+

The data cards necessary to input this general element are shown below:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GENEL   |  629  |       |   1   |   1   |   13  |   4   |   42  |   0   | X1  |
|  +1    |   24  |   2   |       |       |       |       |       |       | X2  |
|  +2    |   UD  |       |   6   |   2   |   33  |   0   |       |       | X3  |
|  +3    |   K   |  1.0  |  2.0  |  3.0  |  4.0  |  5.0  |  6.0  |  7.0  | X4  |
|  +4    |  8.0  |  9.0  |  0.0  |       |       |       |       |       | X5  |
|  +5    |   S   |  1.5  |  2.5  |  3.5  |  4.5  |  5.5  |  6.5  |  7.5  | X6  |
|  +6    |  8.5  |       |       |       |       |       |       |       |     |


GRAV - Gravity Vector
=====================

## Description

Used to define gravity vectors for use in determining gravity loading for the
structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRAV    |  SID  |  CID  |   G   |   N1  |   N2  |   N3  |       |       |     |
|GRAV    |   1   |   3   |  32.2 |  0.0  |  0.0  | -1.0  |       |       |     |

Field       Contents
-----       --------
SID         Set identification number (Integer > 0).

CID         Coordinate system identification number (Integer >= 0).

G           Gravity vector scale factor (Real).

N1, N2, N3  Gravity vector components (Real; N1**2 + N2**2 + N3**2 > 0.0).

## Remarks

1. The gravity vector is defined by

   ->
   g  =  G*(N1, N2, N3).

2. A CID of zero references the basic coordinate system.

3. Gravity loads may be combined with simple loads (for example, FORCE,
   MOMENT) only by specification on a LOAD card. That is, the SID on a GRAV
   card may not be the same as that on a simple load card.

4. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.


GRDSET - Grid Point Default
===========================

## Description

Defines default options for fields 3, 7, and 8 of all GRID cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRDSET  |       |   CP  |       |       |       |   CD  |   PS  |       |     |
|GRDSET  |       |   16  |       |       |       |   32  |  3456 |       |     |

Field       Contents
-----       --------
CP          Identification number of default coordinate system in which the
            locations of the grid points are defined (Integer >= 0).

CD          Identification number of default coordinate system in which
            displacements are measured at grid points (Integer >= 0).

PS          Permanent single-point constraints associated with grid point (any
            of the digits 1 - 6 with no imbedded blanks) (Integer >= 0).

## Remarks

1. The contents of fields 3, 7, or 8 of this card are assumed for the
   corresponding fields of any GRID card whose fields 3, 7, and 8 are blank.
   If any of these fields on the GRID card are blank, the default option
   defined by this card occurs for that field. If no permanent single-point
   constraints are desired or one of the coordinate systems is basic, the
   default may be overridden on the GRID card making one of fields 3, 7, or 8
   zero (rather than blank). Only one GRDSET card may appear in the Bulk Data
   Deck.

2. The primary purpose of this card is to minimize the burden of preparing
   data for problems with a large amount of repetition (for example,
   two-dimensional pinned-joint problems).

3. At least one of the entries CP, CD, or PS must be nonzero.


GRID - Grid Point
=================

## Description

Defines the location of a geometric grid point of the structural model, the
directions of its displacement, and its permanent single-point constraints.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRID    |   ID  |   CP  |   X1  |   X2  |   X3  |   CD  |   PS  |       |     |
|GRID    |   2   |   3   |  1.0  |  2.0  |  3.0  |       |  316  |       |     |

Field       Contents
-----       --------
ID          Grid point identification number (0 < Integer).

CP          Identification number of coordinate system in which the location
            of the gridpoint is defined (Integer >= 0 or blank). (See the
            GRDSET card for default options for fields 3, 7, and 8.)

X1, X2, X3  Location of the grid point in coordinate system CP (Real).

CD          Identification number of coordinate system in which displacements,
            degrees of freedom, constraints, and solution vectors are defined
            at the grid point (Integer >= 0 or blank). (See the GRDSET card
            for default options for fields 3, 7, and 8.)

PS          Permanent single-point constraints associated with grid point (any
            of the digits 1 - 6 with no imbedded blanks) (Integer >= 0 or
            blank). (See the GRDSET card for default options for fields 3, 7,
            and 8.)

## Remarks

1. Each grid point identification number must be unique with respect to all
   other structural, scalar, and fluid points.

2. The meaning of X1, X2, and X3 depend on the type of coordinate system, CP,
   as follows (see CORDxx card descriptions):

+----------------------------------------------------+
|     Type           X1           X2           X3    |
+----------------------------------------------------|
|  Rectangular       X            Y            Z     |
|  Cylindrical       R       é(degrees)        Z     |
|  Spherical         R       é(degrees)   è(degrees) |
+----------------------------------------------------+

3. The collection of all CD coordinate systems defined on all GRID cards is
   called the global coordinate system. All degrees-of-freedom, constraints,
   and solution vectors are expressed in the global coordinate system.


GRIDB - Axisymmetric Problem Grid Point
=======================================

## Description

Defines the location of a geometric grid point on a RINGFL for an axisymmetric
fluid model and/or axisymmetric structure. Used to define the boundary of the
fluid.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRIDB   |   ID  |       |       |  í   |       |   CD  |   PS  |  IDF  |     |
|GRIDB   |   30  |       |       |  30.0 |       |   3   |  345  |   20  |     |

Field       Contents
-----       --------
ID          Grid point identification number (Integer > 0).

í           Azimuthal position in the fluid in degrees (Real).

CD          Identification number of the coordinate system in which
            displacements are defined at the grid point (Integer >= 0).

PS          Permanent single-point constraints associated with the grid point
            (any combination of the digits 1 - 6 with no embedded blanks)
            (Integer >= 0).

IDF         Identification number of a RINGFL (Integer > 0).

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. Each GRIDB identification number must be unique with respect to other
   scalar, structural, and fluid points.

3. An AXIF card must define a fluid coordinate system.

4. The RINGFL referenced must be present.

5. If no harmonic numbers on the AXIF card are specified, no fluid elements
   are necessary.

6. The collection of all CD coordinate systems defined on all GRID and GRIDB
   cards is called the global coordinate system.

7. Fields 3, 4, and 6 are ignored. This will facilitate your conversion of
   GRID cards to GRIDB cards. Note that the fields are the same except for
   fields 1 and 9 if a cylindrical coordinate system is used.

8. The referenced RINGFL point must be included in a boundary list (BDYLIST
   data card).


GRIDF - Fluid Point
===================

## Description

Defines a scalar degree of freedom for harmonic analysis of a fluid.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRIDF   |   ID  |   R   |   Z   |       |       |       |       |       |     |
|GRIDF   |   23  |  2.5  | -7.3  |       |       |       |       |       |     |

Field       Contents

ID          Identification number of axisymmetric fluid point (Integer > 0).

R           Radial location of point in basic coordinate system (Real > 0.0).

Z           Axial location of point in basic coordinate system (Real).

## Remarks

1. This card is allowed only if an AXSLOT card is also present.

2. The identification number (ID) must be unique with respect to all other
   scalar,  structural, and fluid points.

3. Grid points on slot boundaries are defined on GRIDS cards. Do not also
   define them on GRIDF cards.

4. For plotting purposes the R location corresponds to the basic X coordinate.
   The Z location corresponds to the basic Y coordinate. Pressures will be
   plotted as displacement in the basic Z direction.

5. Load and constraint conditions are applied as if the GRIDF is a scalar
   point. Positive loads correspond to inward flow and a single point
   constraint causes zero pressure at the point.


GRIDS - Slot Surface Point
==========================

## Description

Defines a scalar degree of freedom with a two dimensional location. Used in
defining pressure in slotted acoustic cavities.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GRIDS   |   ID  |   R   |   Z   |   W   |  IDF  |       |       |       |     |
|GRIDS   |   25  |  2.5  | -7.3  |  0.5  |       |       |       |       |     |

Field       Contents

ID          Identification number of slot point (Integer > 0).

R           Radial location of point in basic coordinate system (Real not
            equal 0.0).

Z           Axial location of point in basic coordinate system (Real).

W           Slot width or thickness at the GRIDS point (Real >= 0.0, or
            blank).

IDF         Identification number to define a GRIDF point (Integer > 0, or
            blank).

## Remarks

1. This card is allowed only if an AXSLOT card is also present.

2. The identification numbers (ID and IDF if present) must be unique with
   respect to all other scalar, structural, and fluid points.

3. If W is blank, the default value on the AXSLOT card will be used.

4. The IDF number is referenced on the CAXIFi card for central cavity fluid
   elements next to the interface. The IDF number is entered only if the grid
   point is on an interface. In this case it should not also be defined on a
   GRIDF card.

5. If IDF is nonzero then R must be greater than zero.

6. For plotting purposes the R location corresponds to the basic X coordinate.
   The Z location corresponds to the basic Y coordinate. The slot width, W,
   corresponds to the basic Z coordinate. The pressure will be plotted in the
   basic Z direction.

7. Load and constraint conditions are applied as if the GRIDS is a scalar
   point. Positive loads correspond to inward flow and a single point
   constraint causes zero pressure at the point.


GTRAN - Grid Point Transformation
=================================

## Description

This card defines the output coordinate system transformation to be applied to
the displacement set of a selected grid point.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GTRAN   |  SID  |  NAME |  GID  |  TRAN |       |       |       |       |     |
|GTRAN   |   44  | GIMBAL| 1067  |   45  |       |       |       |       |     |

Field       Contents

SID         Identification number of the transformation set (Integer > 0).

NAME        Basic substructure name (BCD).

GID         Grid point identification (Integer > 0).

TRAN        Identification number of a TRANS bulk data card (Integer >= 0).

## Remarks

1. If TRAN = 0, the displacement set at the grid point will be transformed to
   the overall basic coordinate system.

2. If TRAN = SID, the point will remain fixed to the substructure (that is, no
   transformation occurs).

3. Otherwise, the displacement set at the grid point will be transformed to
   the coordinate system directions defined by the selected TRANS card.

4. Transformation sets must be selected in the Substructure Control Deck (TRAN
   = SID) to be used by NASTRAN. Note that TRAN is a subcommand of the
   substructure COMBINE command.

5. You are cautioned to review all actions to be enabled by this GID to ensure
   that they are defined in terms of this revamped displacement coordinate
   system.


GUST - Aerodynamic Gust Load Description
========================================

## Description

Defines a stationary vertical gust for use in aeroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|GUST    |  SID  | DLOAD |  WG   |  X0   |   V   |       |       |       |     |
|GUST    |  133  |   61  | 1.0   |   0.  |  1.+4 |       |       |       |     |

Field       Contents
-----       --------
SID         Gust set identification number (Integer > 0).

DLOAD       The SID of a TLOAD or RLOAD data card which defines the time or
            frequency dependence (Integer > 0).

WG          Scale factor (gust velocity/forward velocity) for gust velocity
            (Real not equal 0.)

X0          Location of reference plane in aerodynamic coordinates (Real >=
            0.0).

V           Velocity of vehicle (Real > 0.0).

## Remarks

1. The GUST card is selected in Case Control by GUST = SID.

2. The gust angle is in the +z direction of the aerodynamic coordinate system.
   The value is

              x-x
                 0
   WG * T(t - ----)
               V

   where T is the tabular function.

3. In random analysis, a unit gust velocity (WG=1/velocity) is suggested. The
   actual rms value is entered on the TABRNDG data card.

4. X0 and V may not change between subcases under one execution.


LOAD - Static Load Combination (Superposition)
==============================================

## Description

Defines a static load as a linear combination of load sets defined via FORCE,
MOMENT, FORCE1, MOMENT1, FORCE2, MOMENT2, PLOAD, PLOAD2, PLOAD3, FORCEAX,
PRESAX, MOMAX, SLOAD, RFORCE, and GRAV cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|LOAD    |  SID  |   S   |   S1  |   L1  |   S2  |   L2  |   S3  |   L3  |abc  |
|+bc     |   S4  |   L4  |       | etc.  |       |       |       |       |     |
|LOAD    |  101  | -0.5  |  1.0  |   3   |  6.2  |   4   |       |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

S           Scale factor (Real).

Si          Scale factors (Real).

Li          Load set identification numbers defined via card types enumerated
            above (Integer > 0).

## Remarks

1. The load vector defined is given by

   {P}  =  S - S  {P  }
             i  i   Li

2. The SID on a LOAD card must be unique and must be different from the load
   set identification numbers of all external static load sets in the Bulk
   Data Deck.

3. The Li must be unique. The remainder of the physical card containing the
   last entry must be blank.

4. This card must be used if gravity loads (GRAV) are to be used with any of
   the other types.

5. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

6. A LOAD card may not reference a set identification number defined by
   another LOAD card.


LOADC - Substructure Static Loading Combination
===============================================

## Description

Defines the static load for a substructuring analysis as a linear combination
of load sets defined for each basic substructure.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|LOADC   |  SID  |   S   | NAME1 |  ID1  |   S1  | NAME2 |  ID2  |   S2  |abc  |
|+bc     |       |       | NAME3 |  ID3  |   S3  | NAME4 |  ID4  |   S4  |def  |
|LOADC   |   27  |  1.0  | WINGRT|   5   |  0.5  |FUSELAG|  966  |  2.5  |ABC  |
|+BC     |       |       | MIDWG |   27  |  1.75 |       etc.    |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

S           Scale factor applied to final load vector (Real).

NAMEi       Basic substructure name (BCD).

IDi         Load set identification number of substructure NAMEi (Integer >
            0).

Si          Scale factor (Real).

## Remarks

1. The load vector is combined by:

   {P}  =  S - Si {P}
             i       IDi

2. The load set identification numbers (IDi) reference the load sets used in
   Phase 1 to generate the load vectors on the basic substructures.

3. The NAMEi and IDi need not be unique.

4. The LOADC card is the means of specifying a static loading condition in a
   Phase 2 substructure analysis. The IDi may actually reference temperature
   loads or element deformation loads defined in Phase 1.

5. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.


MAT1 - Material Property Definition
===================================

## Description

Defines the material properties for linear, temperature-independent, isotropic
materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT1    |  MID  |   E   |   G   |   NU  |  RHO  |   A   |  TREF |   GE  |+abc |
|+abc    |   ST  |   SC  |   SS  | MCSID |       |       |       |       |     |
|MAT1    |   17  | 3.+7  | 1.9+7 |       | 4.28  | 0.19  | 5.37+2|  0.23 |ABC  |
|+BC     | 20.+4 | 15.+4 | 12.+4 | 2004  |       |       |       |       |     |

Field       Contents
-----       --------
MID         Material identification number (Integer > 0).

E           Young's modulus (Real >= 0.0 or blank).

G           Shear modulus (Real >= 0.0 or blank).

NU          Poisson's ratio (-1.0 < Real <= 0.5 or blank).

RHO         Mass density (Real).

A           Thermal expansion coefficient (Real).

TREF        Thermal expansion reference temperature (Real).

GE          Structural element damping coefficient (Real).

ST, SC, SS  Stress limits for tension, compression, and shear (Real) (Required
            for property optimization calculations; otherwise optional if
            margins of safety are desired.)

MCSID       Material coordinate system identification number (Integer >= 0 or
            blank).

## Remarks

1. One of E or G must be positive (that is, either E > 0.0 or G > 0.0 or both
   E and G may be > 0.0).

2. If any one of E, G, or NU is blank, it will be computed to satisfy the
   identity E = 2(1+NU)G; otherwise, values supplied by you will be used.

3. The material identification number must be unique for all MAT1, MAT2, and
   MAT3 cards.

4. MAT1 materials may be made temperature dependent by use of the MATT1 card
   and stress dependent by use of the MATS1 card.

5. The mass density, RHO, will be used to automatically compute mass for all
   structural elements except the two-dimensional bending only elements TRBSC,
   TRPLT, and QDPLT.

6. If E and NU or G and NU are both blank they will be given the value 0.0.

7. Weight density may be used in field 6 if the value 1/g is entered on the
   PARAM card WTMASS, where g is the acceleration of gravity.

8. Solid elements must not have NU equal to 0.5.

9. Entries for A (thermal expansion coefficient) and TREF (reference
   temperature) are assumed to be 0.0 when blank. In a heat formulation, A
   must be overridden by an appropriate entry; TREF may be overridden if
   desired.

10.   MCSID (> 0) is required if stresses or strains/curvatures are to be
      computed in a material coordinate system. This is applicable only for
      TRIA1, TRIA2, QUAD1, and QUAD2 elements.


MAT2 - Material Property Definition
===================================

## Description

Defines the material properties for linear, temperature-independent,
anisotropic materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT2    |  MID  |  G11  |  G12  |  G13  |  G22  |  G23  |  G33  |  RHO  |+abc |
|+abc    |   A1  |   A2  |  A12  |   T0  |   GE  |   ST  |   SC  |   SS  |+def |
|+def    | MCSID |       |       |       |       |       |       |       |     |
|MAT2    |   13  | 6.2+3 |       |       | 6.2+3 |       | 5.1+3 | 0.056 |ABC  |
|+BC     | 0.15  |       |       |-500.0 | 0.002 | 20.+5 |       |       |DEF  |
|+BC     |  1008 |       |       |       |       |       |       |       |     |

Field       Contents

MID         Material identification number (Integer > 0).

Gij         The material property matrix (Real).

RHO         Mass density (Real).

Ai          Thermal expansion coefficient vector (Real).

T0          Thermal expansion reference temperature (Real).

GE          Structural element damping coefficient (Real).

ST, SC, SS  Stress limits for tension, compression, and shear (Real). (Used
            only to compute margins of safety in certain elements; they have
            no effect on the computational procedures.)

MCSID       Material coordinate system identification number (Integer >= 0 or
            blank).

## Remarks

1. The material identification numbers must be unique for all MAT1, MAT2, and
   MAT3 cards.

2. MAT2 materials may be made temperature dependent by use of the MATT2 card.

3. The mass density, RHO, will be used to automatically compute mass for all
   structural elements except the two-dimensional bending only elements TRBSC,
   TRPLT, and QDPLT.

4. The convention for the Gij in fields 3 through 8 is represented by the
   following matrix relationship.

   É    »   +-                  -+  É    »
   º +1 º   | G11     G12    G13 |  º îl º
   º    º   |                    |  º    º
   º +2 º = | G12     G22    G23 |  º î2 º
   º    º   |                    |  º    º
   º ç12º   | G13     G23    G33 |  º +12º
   È    ¼   +-                  -+  È    ¼

5. MCSID (> 0) is required if stresses or strains/curvatures are to be
   computed in a material coordinate system. This is applicable only for
   TRIA1, TRIA2, QUAD1, and QUAD2 elements.


MAT3 - Material Property Definition
===================================

## Description

Defines the material properties for linear, temperature-independent,
orthotropic materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT3    |  MID  |   EX  |   EY  |   EZ  |  NUXY |  NUYZ |  NUZX |  RHO  |+abc |
|+abc    |  GXY  |  GYZ  |  GZX  |   AX  |   AY  |   AZ  |  TREF |   GE  |     |
|MAT3    |   23  | 1.0+7 | 1.1+7 | 1.2+7 |   .3  |  .25  |  .27  | 1.0-5 |ABC  |
|+BC     | 2.5+6 | 3.0+6 | 2.5+6 | 1.0-4 | 1.0-4 | 1.1-4 |  68.5 |  .23  |     |

Field       Contents

MID         Material identification number (Integer > 0).

EX, EY, EZ  Young's moduli in the x, y, and z directions respectively (Real >=
            0.0).

NUXY, NUYZ, NUZX  Poisson's Ratios (Coupled strain ratios in the xy, yz, and 
            zx directions respectively) (Real).

RHO         Mass density (Real).

GXY, GYZ, GZX  Shear moduli for xy, yz, and zx (Real >= 0.0).

AX, AY, AZ  Thermal expansion coefficients (Real).

TREF        Thermal expansion reference temperature (Real).

GE          Structural element damping coefficient (Real).

## Remarks

1. The material identification number must be unique with respect to the
   collection of all MATi cards.

2. MAT3 materials may be made temperature-dependent by use of the MATT3 card.

3. All nine of the numbers EX, EY, EZ, NUXY, NUYZ, NUZX, GXY, GYZ, and GZX
   must be present.

4. A nonfatal warning diagnostic will occur if any of NUXY or NUYZ has an
   absolute value greater than 1.0.

5. MAT3 materials may only be referenced by CTRIARG, CTRAPRG, CTRIAAX,
   CTRAPAX, and PTORDRG cards.

6. The mass density, RHO, will be used to automatically compute mass for the
   TRIARG, TRAPRG, CTRIAAX, CTRAPAX, and TORDRG elements.


MAT4 - Thermal Material Property Definition
===========================================

## Description

Defines the thermal material properties for temperature-independent, isotropic
materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT4    |  MID  |   K   |   CP  |       |       |       |       |       |     |
|MAT4    |  103  |  .6   |   .2  |       |       |       |       |       |     |

Field       Contents

MID         Material identification number (Integer > 0).

K           Thermal conductivity (Real > 0.0), or convective film coefficient.

CP          Thermal capacity per unit volume (Real > 0.0 or blank), or film
            capacity per unit area.

## Remarks

1. The material identification number may be the same as a MAT1, MAT2, or MAT3
   card, but must be unique with respect to other MAT4 or MAT5 cards.

2. If an HBDY element references this card, K is the convective film
   coefficient and CP is the thermal capacity per unit area.

3. MAT4 materials may be made temperature dependent by use of the MATT4 card.


MAT5 - Thermal Material Property Definition
===========================================

## Description

Defines the thermal material properties for temperature-independent,
anisotropic materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT5    |  MID  |  KXX  |  KXY  |  KXZ  |  KYY  |  KYZ  |  KZZ  |   CP  |     |
|MAT5    |   24  | .092  |       |       | .083  |       | .020  |  0.2  |     |

Field       Contents

MID         Material identification number (Integer > 0).

KXX, KXY, KXZ, KYY, KYZ, KZZ  Thermal conductivity matrix terms (Real).

CP          Thermal capacity per unit volume (Real >= 0.0 or blank).

## Remarks

1. The thermal conductivity matrix has the form:

          +--            ---+
          | KXX   KXY   KXZ |
          |                 |
   K =    | KXY   KYY   KYZ |
          |                 |
          | KXZ   KYZ   KZZ |
          +--            ---+

2. The material number may be the same as a MAT1, MAT2, or MAT3 card, but must
   be unique with respect to the MAT4 or MAT5 cards.

3. MAT5 materials may be made temperature dependent by use of the MATT5 card.


MAT6 - Material Property Definition
===================================

## Description

Defines the material properties for linear, temperature-independent,
anisotropic materials for solid isoparametric elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT6    |  MID  |  G11  |  G12  |  G13  |  G14  |  G15  |  G16  |  G22  |+a   |
|+a      |  G23  |  G24  |  G25  |  G26  |  G33  |  G34  |  G35  |  G36  |+b   |
|+b      |  G44  |  G45  |  G46  |  G55  |  G56  |  G66  |  RHO  |  AXX  |+c   |
|+c      |  AYY  |  AZZ  |  AXY  |  AY2  |  AZX  |  TREF |    GE |       |     |
|MAT6    |   31  |0.23+7 |-0.21+7| 0.32+6| 0.16+7| 0.11+7| 0.53+6| 0.74+7|+A   |
|+A      |-0.21+7|-0.55+7|-0.37+7|-0.18+7| 0.23+7| 0.16+7| 0.11+7| 0.53+6|+B   |
|+B      | 0.66+7| 0.28+7| 0.14+7| 0.43+7| 0.92+6| 0.30+7| 7.32-4|       |     |

Field       Contents

MID         Material property identification number (Integer > 0).

Gij         Symmetric portion of 6x6 material matrix (Real).

RHO         Mass density (Real).

Aij         Thermal expansion coefficient vector (Real).

TREF        Thermal expansion reference temperature (Real).

GE          Structural damping coefficient (Real).

## Remarks

1. The material property identification number must be unique with respect to
   all other material cards.

2. MAT6 materials may be made temperature-dependent by use of the MATT6 card.

3. The ordering of the rows and columns of the matrix is critical and must
   conform to NASTRAN's ordering of the stress and strain vectors.


MAT8 - Orthotropic Plate Material Property Definition
=====================================================

## Description

Defines the material property for an orthotropic material for plate elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MAT8    |  MID  |  E1   |  E2   |  NU12 |  G12  |  G1Z  |   G2Z |  RH0  |abc  |
|+bc     |  A1   |  A2   | TREF  |  XT   |  XC   |  YT   |  YC   |   S   |def  |
|+ef     |  GE   |  F12  |       |       |       |       |       |       |     |
|MAT8    |  299  | 32.+6 | 4.2+5 |  0.33 | 2.9+6 |       |       | 0.042 |ABC  |
|+BC     | 14.-6 | 2.3-6 |  175. |       |       |       |       |       |DEF  |
|+EF     |  2.5-4|       |       |       |       |       |       |       |     |

Field       Contents
-----       --------
MID         Material identification number (Integer > 0).

E1, E2      Modulus of elasticity in the material x and y directions (Real not
            equal 0.0).

NU12        Poisson's Ratio (Real) (See Remark 5).

G12         Linear in-plane shear modulus (Real > 0.0).

G1Z         Transverse shear modulus for shear in X-Z plane (Real).

G2Z         Transverse shear modulus for shear in Y-Z plane (Real).

RHO         Mass density (Real).

A1, A2      Thermal expansion coefficients in the material x and y directions
            (T, Real > 0.0).

TREF        Thermal expansion reference temperature (XC, Real).

XT, XC      Allowable stresses/strains in tension and compression,
            respectively, in the material x direction. Required if failure
            index calculation is desired. (XT, Real > 0.0; XC, Real; default
            value for XC is XT.) (See Remark 3.)

YT, YC      Allowable stresses/strains in tension and compression,
            respectively, in the material y direction. Required if failure
            index calculation is desired. (YT, Real > 0.0; YC, Real; default
            value for YC Is YT.) (See Remark 3.)

S           Allowable stress/strain for in-plane shear (Real > 0.0) (See
            Remark 3.)

GE          Structural damping coefficient (Real).

F12         Tsai-Wu interaction term (Real) (See Remark 4.)

## Remarks

1. Material coordinate systems are defined by the plate element connection
   entries on the CQUAD4 and CTRIA3 cards.

2. The stress-strain relationship defined by this data is:

+-------------------------------------------------------------+
|          +                         +                        |
|  î       |  1/E1   -NU12/E1    é   |   +                A1  |
|   1      |                         |    1                   |
|          |                         |                        |
|  î    =  |-NU12/E1    1/E2     é   |   +    + (T-TREF)  A2  |
|   2      |                         |    2                   |
|          |                         |                        |
|  ç       |    é         é    1/G12 |   +                é   |
|   12     |                         |    12                  |
|          +                         +                        |
|                                                             |
+-------------------------------------------------------------+

+-------------------------------------------+
|               +--        --+              |
|    +          |  G1Z    é  |      ç       |
|     xz        |            |       xz     |
|           =   |            |              |
|    +          |   é    G2Z |      ç       |
|     yz        |            |       yz     |
|               +---       --+              |
+-------------------------------------------+

3. Fields XT, XC, YT, YC, and S are used only for composite materials when
   failure calculations are requested with PCOMP, PCOMP1, or PCOMP2 Bulk Data
   entries. Allowables represent stresses except when the maximum strain
   failure theory is used.

4. The F12 field is used only for composite materials when the Tsai-Wu failure
   theory is used and failure calculations are requested.

5. NU12 is Poisson's Ratio (î1/î2 for uniaxial loading in 1-direction). Note
   that NU21 = î1/î2, uniaxial loading in 2-direction, is related to NU12, E1,
   and E2 by the relationship, (NU12) (E2) = (NU12) (E1).


MATF - Fluid Material Property Definition
=========================================

## Description

Defines the fluid density in a hydroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATF    |  MID  |  RHO  |       |       |       |       |       |       |     |
|MATF    |  103  |  0.6  |       |       |       |       |       |       |     |

Field       Contents

MID         Material identification number (Integer > 0).

RHO         Mass density (Real > 0.0).

## Remarks

1. The material identification number may be the same as that of a MAT1, MAT2,
   or MAT3 card, but must be unique with respect to other MATF cards.

MATPZ1 - Piezoelectric Material Property Definition
===================================================

## Description

Defines the material properties for linear, temperature-independent
piezoelectric materials.

## Format and Example

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|        |       |   E   |   E   |   E   |   E   |   E   |       |       |     |
|MATPZ1  |  MID  |  S    |  S    |  S    |  S    |  S    |  d    |  d    |+a   |
|        |       |   11  |   33  |   44  |   12  |   13  |   31  |   33  |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|MATPZ1  |   1   | 12.3  | 15.5  | 39.0  | -4.05 | -5.31 |-123.0 | 289.0 |+A   |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|        |       | S     | S     |       |       |       |       |       |     |
|+a      |  d15  |î  /î  |î  /î  |  RHO  |   A   | TREF  |  GE   |       |     |
|        |       | 11  0 | 33  0 |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+A      | 496.0 | 730.0 | 635.0 | 7500.0|       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field       Contents

MID         Material identification number (Integer > 0).

SE11 - d15  Piezoelectric constants multiplied by 10**12 (Real).

îS11/î0, îS33/î0  Piezoelectric constants, where î0 is taken to be 8.854 x
            10**(-12) farad/meter (Real).

RHO         Mass density (Real).

A           Thermal expansion coefficient (Real).

TREF        Thermal expansion reference temperature (Real).

GE          Structural element damping coefficient (Real).

## Remarks

1. MID must be unique with respect to all other material cards.

2. MATPZ1 materials may be made temperature-dependent by use of the MTTPZ1
   card.

3. MATPZ1 may be referenced only by PTRAPAX and PTRIAAX cards.

4. Matrix [SE] must be nonsingular.


MATPZ2 - Piezoelectric Material Property Definition
===================================================

## Description

Defines the material properties for linear, temperature-independent,
piezoelectric materials.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATPZ2  |  MID  | CE11  | CE12  | CE13  | CE14  | CE15  | CE16  | CE22  |+a   |
|+a      |  CE23 | CE24  | CE25  | CE26  | CE33  | CE34  | CE35  | CE36  |+b   |
|+b      |  CE44 | CE45  | CE46  | CE55  | CE56  | CE66  |  E11  |  E12  |+c   |
|+c      |  E13  |  E14  |  E15  |  E16  |  E21  |  E22  |  E23  |  E24  |+d   |
|+d      |  E25  |  E26  |  E31  |  E32  |  E33  |  E34  |  E35  |  E36  |+e   |
|+e      | EPS11 | EPS12 | EPS13 | EPS22 | EPS23 | EPS33 |  RHO  |   AX  |+f   |
|+f      |   AY  |  AZ   |  TREF |  GE   |       |       |       |       |     |
|MATPZ2  |  23   |   1.  |   2.  |   3.  |   4.  |   5.  |   6.  |   1.  |+A   |
|+A      |   2.  |   3.  |   4.  |   5.  |   1.  |   2.  |   3.  |   4.  |+A   |
|+B      |   1.  |   2.  |   3.  |   1.  |   2.  |   1.  |   1.  |   2.  |+C   |
|+C      |   3.  |   4.  |   5.  |   6.  |   1.  |   2.  |   3.  |   4.  |+D   |
|+D      |   5.  |   6.  |   1.  |   2.  |   3.  |   4.  |   5.  |   6.  |+E   |
|+E      |   1.  |   2.  |   3.  |   4.  |   5.  |   6.  |  .15  | 6.-7  |+F   |
|+F      | 6.-7  | 6.-7  |  70.  |  .2   |       |       |       |       |     |

Field       Contents
-----       --------
MID         Material identification number (Integer > 0).

CE11 - EPS33  Piezoelectric constants (Real).

RHO         Mass density (Real).

AX, AY, AZ  Thermal expansion coefficients (Real).

TREF        Thermal expansion reference temperature (Real).

GE          Structural element damping coefficient (Real).

## Remarks

1. MID must be unique with respect to all other material cards.

2. MATPZ2 materials may be made temperature-dependent by use of the MTTPZ2
   card.

3. MATPZ2 may be referenced only by PTRAPAX and PTRIAAX cards.

4. See CAUTION discussed in Section 1.17.3.2.


MATS1 - Material Stress Dependence
==================================

## Description

Specifies table references for material properties on a MAT1 card that are
stress-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATS1   |  MID  |   R1  |       |       |       |       |       |       |     |
|MATS1   |   17  |   28  |       |       |       |       |       |       |     |

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MAT1 card (Integer > 0).

R1          Reference to table identification number (Integer >= 0 or blank).

## Remarks

1. A blank or zero entry means no table dependence of the referenced quantity,
   E, on the basic MAT1 card. For this case, the MATS1 card is not required.

2. TABLES1 type tables must be used.


MATT1 - Material Temperature Dependence
=======================================

## Description

Specifies table references for isotropic material properties on a MAT1 card
that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT1   |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+abc |
|+abc    |   R8  |   R9  |  R10  |       |       |       |       |       |     |
|MATT1   |   17  |   32  |       |       |       |   15  |       |       |ABC  |
|+BC     |   62  |       |       |       |       |       |       |       |     |

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MAT1 card (Integer > 0).

Ri          References to table identification numbers (Integer > 0 or blank)
            for the corresponding fields on the MAT1 card.

## Remarks

1. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MAT1 card, and the quantity remains constant.

2. TABLEM1, TABLEM2, TABLEM3, or TABLEM4 type tables may be used.

3. Material properties given on a basic MATi card are initial values. If two
   or more quantities are to retain a fixed relationship, then two or more (as
   required) tables must be input to define the relationship.


MATT2 - Material Temperature Dependence
=======================================

## Description

Specifies table references for anisotropic material properties on a MAT2 card
that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT2   |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+abc |
|+abc    |   R8  |   R9  |  R10  |  R11  |  R12  |  R13  |  R14  |  R15  |     |
|MATT2   |   17  |   32  |       |       |       |   15  |       |       |ABC  |
|+BC     |   62  |       |       |       |       |       |       |       |     |

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MAT2 card (Integer > 0).

Ri          References to table identification numbers (Integer >= 0 or blank)
            for the corresponding fields on the MAT2 card.

## Remarks

1. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MAT2 card, and the quantity remains constant.

2. TABLEM1, TABLEM2, TABLEM3, or TABLEM4 type tables may be used.

3. Material properties given on a basic MATi card are initial values. If two
   or more quantities are to retain a fixed relationship, then two or more (as
   required) tables must be input to define the relationship.


MATT3 - Material Temperature Dependence
=======================================

## Description

Specifies table references for orthotropic material properties on a MAT3 card
that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT3   |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+abc |
|+abc    |   R8  |   R9  |  R10  |  R11  |  R12  |  R13  |  R14  |  R15  |     |
|MATT3   |   23  |   48  |       |       |   54  |       |       |       |ABC  |
|+BC     |   74  |       |       |       |       |       |       |       |     |

Field       Contents
-----       --------
MID         Material property identification number which matches the
            identification number on some basic MAT3 card (Integer > 0).

Ri          References to table identification numbers (Integer > 0 or blank)
            for the corresponding fields on the MAT3 card.

## Remarks

1. Blank or zero entries imply no table dependence of the referenced quantity
   on the basic MAT3 card, and the quantity remains constant.

2. TABLEM1, TABLEM2, TABLEM3, or TABLEM4 type tables may be used.

3. Material properties given on a basic MATi card are initial values. If two
   or more quantities are to retain a fixed relationship, then two or more (as
   required) tables must be input to define the relationship.


MATT4 - Thermal Material Temperature Dependence
===============================================

## Description

Specifies table reference for temperature dependent thermal conductivity or
convective film coefficient on a MAT4 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT4   |  MID  |  T(K) |       |       |       |       |       |       |     |
|MATT4   |  103  |   73  |       |       |       |       |       |       |     |

Field       Contents
-----       --------
MID         ID of a MAT4 which is to be temperature dependent (Integer > 0).

T(K)        Identification number of a TABLEMi card which gives temperature
            dependence of the thermal conductivity or convective film
            coefficient (Integer >= 0 or blank).

## Remarks

1. The thermal capacity may not be temperature dependent; field 4 must be
   blank.

2. TABLEM1, TABLEM2, TABLEM3, or TABLEM4 type tables may be used. The basic
   quantity, K, on the MAT4 card is always multiplied by the tabular function.
   Note that this is different from structural applications.

3. A blank or zero entry means no table dependence of the referenced quantity
   on the basic MAT4 card. For this case, the MATT4 card is not required.


MATT5 - Thermal Material Temperature Dependence
===============================================

## Description

Specifies table references for thermal conductivity matrix terms on a MAT5
card that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT5   |  MID  |T(KXX) |T(KXY) |T(KXZ) |T(KYY) |T(KYZ) |T(KZZ) |       |     |
|MATT5   |  24   |  73   |       |       |       |       |       |       |     |

Field       Contents
-----       --------
MID         Identification number of a MATS, which is to be temperature
            dependent (Integer > 0).

T(K--)      Identification number of a TABLEMi card which gives temperature
            dependence of the matrix term (Integer >= 0 or blank).

## Remarks

1. The thermal capacity may not be temperature dependent. Field 9 must be
   blank.

2. TABLEM1, TABLEM2, TABLEM3, or TABLEM4 type tables may be used. The basic
   quantities on the MAT5 card are always multiplied by the tabular function.
   Note that this is different from the structural applications.

3. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MAT5 card, and the quantity remains constant.

4. Material properties given on a basic MATi card are initial values. If two
   or more quantities are to retain a fixed relationship, then two or more (as
   required) tables must be input to define the relationship.


MATT6 - Material Temperature Dependence
=======================================

## Description

Specifies table references for material properties on a MAT6 card that are
temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MATT6   |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+a   |
|+a      |   R8  |   R9  |  R10  |  R11  |  R12  |  R13  |  R14  |  R15  |+b   |
|+b      |  R16  |  R17  |  R18  |  R19  |  R20  |  R21  |  R22  |  R23  |+c   |
|+c      |  R24  |  R25  |  R26  |  R27  |  R28  |  R29  |  R30  |       |     |
|MATT6   |  115  |   101 |   102 |   103 |   104 |   105 |   106 |   107 |+A   |
|+A      |   108 |   109 |  110  |  111  |  112  |  113  |  114  |  115  |+B   |
|+B      |  116  |  117  |  118  |  119  |  120  |  121  |  122  |  123  |+C   |
|+C      |  124  |  125  |  126  |  127  |  128  |  129  |  130  |       |     |

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MAT6 card (Integer > 0).

Ri          References to table identification numbers (Integer >= 0 or
            blank).

## Remarks

1. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MAT6 card.

2. TABLEM1, TABLEM2, TABLEM3, and TABLEM4 type tables may be used.

MDIPOLE - Magnetic Dipole Moment
================================

## Description

Defines a magnetic dipole moment in magnetic field problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MDIPOLE |  SID  |  CID  |  CX   |  CY   |  CZ   |  MX   |  MY   |  MZ   |+a   |
|+a      |  MIN  |  MAX  |       |       |       |       |       |       |     |
|MDIPOLE |   5   |       |  1.0  |  2.1  |  3.0  | 10.0  | 20.0  | 30.0  |+A   |
|+A      |  0.0  |  0.0  |       |       |       |       |       |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

CID         Coordinate system identification number (Integer > 0).

CX, CY, CZ  Coordinates of location of dipole in coordinate system CID (Real).

MX, MY, MZ  Components of magnetic dipole moment in coordinate system CID
            (Real).

MIN         Minimum distance from dipole to grid point for computing magnetic
            equivalent loads (Real > 0.0).

MAX         Maximum distance from dipole to grid point for computing magnetic
            equivalent loads (Real > 0.0).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. Presently, CID must be blank or zero, indicating the basic coordinate
   system.

3. MIN and MAX represent minimum and maximum distances, respectively, from the
   dipole to a point outside of which the magnetic equivalent loads will not
   be computed for this dipole. If MAX is zero or blank, loads for all
   necessary points beyond the MIN distance will be computed.

4. The continuation card is required.


MKAERO1 - Mach Number, Frequency Table
======================================

## Description

Provides a table of Mach numbers or interblade phase angles (m) and reduced
frequencies (k) for aerodynamic matrix calculation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MKAERO1 |   M1  |   M2  |   M3  |   M4  |   M5  |   M6  |   M7  |   M8  |ABC  |
|+BC     |   K1  |   K2  |   K3  |   K4  |   K5  |   K6  |   K7  |   K8  |     |
|MKAERO1 |   .1  |   .7  |       |       |       |       |       |       |+ABC |
|+BC     |   .3  |   .6  |  1.0  |       |       |       |       |       |     |

Field       Contents
-----       --------
Mi          List of Mach numbers or interblade phase angles (Real; 1 <= i <=
            8). See Remark 5.

Kj          List of reduced frequencies (Real > 0.0, 1 <= j <= 8).

## Remarks

1. Blank fields end the list, and thus cannot be used for 0.0.

2. All combinations of (M,K) will be used.

3. The continuation card is required.

4. Since 0.0 is not allowed, it may be simulated with a very small number such
   as 0.0001.

5. Mach numbers are input for wing flutter analysis and interblade phase
   angles for blade flutter analysis.


MKAERO2 - Mach Number, Frequency Table
======================================

## Description

Provides a list of Mach numbers or interblade phase angles (m) and reduced
frequencies (k) for aerodynamic matrix calculation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MKAERO2 |   M1  |   K1  |   M2  |   K2  |   M3  |   K3  |   M4  |   K4  |     |
|MKAERO2 |  .10  |  .30  |  .10  |  .60  |  .70  |  .30  |  .70  |  1.0  |     |

Field       Contents
-----       --------
Mi          List of Mach numbers or interblade phase angles (Real > 0.0). See
            Remark 4.

Ki          List of reduced frequencies (Real > 0.0).

## Remarks

1. This card will cause the aerodynamic matrices to be computed for a set of
   parameter pairs.

2. Several MKAERO2 cards may be in the deck.

3. Imbedded blank pairs are skipped.

4. Mach numbers are input for wing flutter analysis and interblade phase
   angles for blade flutter analysis.


MOMAX - Conical Shell Static Moment
===================================

## Description

Defines a static moment loading of a conical shell coordinate.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MOMAX   |  SID  |  RID  |  HID  |   S   |   MR  |   MP  |   MZ  |       |     |
|MOMAX   |   1   |   2   |   3   |  1.0  |  0.1  |  0.2  |  0.3  |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

RID         Ring identification number (see RINGAX) (Integer > 0).

HID         Harmonic identification number (Integer >= 0 or a sequence of
            harmonics; see Remark 5).

S           Scale factor (Real).

MR, MP, MZ  Moment components in the r, í, z directions (Real).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

3. A separate card is needed for the definition of the moment associated with
   each harmonic.

4. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

5. If a sequence of harmonics is to be placed in HID the form is as follows:
   "Sn1Tn2" where n1 is the start of the sequence and n2 is the end of the
   sequence, that is, for harmonics 0 through 10, the field would contain
   "S0T10".


MOMENT - Static Moment
======================

## Description

Defines a static moment at a grid point by specifying a vector.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MOMENT  |  SID  |   G   |  CID  |   M   |   N1  |   N2  |   N3  |       |     |
|MOMENT  |   2   |   5   |   6   |  2.9  |  0.0  |  1.0  |  0.0  |       |     |

Field       Contents
-----       --------
SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

CID         Coordinate system identification number (Integer >= 0).

M           Scale factor (Real).

N1, N2, N3  Components of vector measured in coordinate system defined by CID
            (Real; N1**2 + N2**2 + N3**2 > 0.0).

## Remarks

1. The static moment applied to grid point G is given by

   ->
   m  =  M*(N1,N2,N3)

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

3. A CID of zero references the basic coordinate system.


MOMENT1 - Static Moment
=======================

## Description

Used to define a static moment by specification of a value and two grid points
which determine the direction.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MOMENT1 |  SID  |   G   |   M   |   G1  |   G2  |       |       |       |     |
|MOMENT1 |   6   |   13  | -2.93 |   16  |   13  |       |       |       |     |

Field       Contents

SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

M           Value of moment (Real).

G1, G2      Grid point identification numbers (Integer > 0; G1 not equal G2).

## Remarks

1. The direction of the moment is determined by the vector from G1 to G2.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

MOMENT2 - Static Moment
=======================

## Description

Used to define a static moment by specification of a value and four grid
points which determine the direction.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MOMENT2 |  SID  |   G   |   M   |   G1  |   G2  |   G3  |   G4  |       |     |
|MOMENT2 |   6   |   13  | -2.93 |   16  |   13  |   17  |   13  |       |     |

Field       Contents

SID         Load set identification number (Integer > 0).

G           Grid point identification number (Integer > 0).

M           Value of moment (Real).

G1,...,G4   Grid point identification numbers (Integer > 0; G1 not equal G2;
            G3 not equal G4).

## Remarks

1. The direction of the force is determined by the vector product whose
   factors are vectors from G1 to G2 and G3 to G4 respectively.

2. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

MPC - Multipoint Constraint
===========================

## Description

Defines a multipoint constraint equation of the form

   - A  u   =  0
   j  j  j

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MPC     |  SID  |   G   |   C   |   A   |   G   |   C   |   A   |       |+abc |
|+bc     |       |   G   |   C   |   A   |  etc. |       |       |       |     |
|MPC     |   3   |   28  |   3   |  6.2  |   2   |       |  4.29 |       |+B   |
|+B      |       |   1   |   4   | -2.91 |       |       |       |       |     |

Field       Contents

SID         Set identification number (Integer > 0).

G           Identification number of grid or scalar point (Integer > 0).

C           Component number - any one of the digits 1 - 6 in the case of
            geometric grid points; blank or zero in the case of scalar points
            (Integer).

A           Coefficient (Real; the first A must be nonzero).

## Remarks

1. The first coordinate in the sequence is assumed to be the dependent
   coordinate and must be unique for all equations of the set.

2. Forces of multipoint constraint are not recovered.

3. Multipoint constraint sets must be selected in the Case Control Deck (MPC =
   SID) to be used by NASTRAN.

4. Dependent coordinates on MPC cards may not appear on OMIT, OMIT1, SUPORT,
   SPC, or SPC1 cards; nor may the dependent coordinates be redundantly
   implied on ASET, ASET1, or MPCADD cards. They also may not appear as
   dependent coordinates in CRIGD1, CRIGD2, CRIGD3, or CRIGDR elements.

MPCADD - Multipoint Constraint Set Definition
=============================================

## Description

Defines a multipoint constraint set as a union of multipoint constraint sets
defined via MPC cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MPCADD  |  SID  |   S1  |   S2  |   S3  |   S4  |   S5  |   S6  |   S7  |abc  |
|+bc     |   S8  |   S9  |     etc.      |       |       |       |       |     |
|MPCADD  |  100  |   2   |   3   |   1   |   6   |   4   |       |       |     |

Field       Contents

SID         Set identification number (Integer > 0; not equal 101 or 102 if
            axisymmetric).

Sj          Set identification numbers of multipoint constraint sets defined
            via MPC cards (Integer > 0; SID not equal Sj).

## Remarks

1. The Sj must be unique.

2. Multipoint constraint sets must be selected in the Case Control Deck (MPC =
   SID) to be used by NASTRAN.

3. Sj may not be the identification number of a multipoint constraint set
   defined by another MPCADD card.

4. Set identification numbers of 101 or 102 cannot be used in axisymmetric
   problems.


MPCAX - Axisymmetric Multipoint Constraint
==========================================

## Description

Defines a multipoint constraint equation of the form

   - A  u   =  0
   j  j  j

for a model containing CCONEAX, CTRAPAX, or CTRIAAX elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MPCAX   |  SID  |       |       |       |  RID  |  HID  |   C   |   A   |+abc |
|+abc    |  RID  |  HID  |   C   |   A   |  RID  |  HID  |   C   |   A   |+def |
|MPCAX   |   32  |       |       |       |   17  |   6   |   1   |  1.0  |+1   |
|+1      |   23  |   4   |   2   | -6.8  |       |       |       |       |     |

Field       Contents

SID         Set identification number (Integer > 0, not equal 101 or 102).

RID         Ring identification number (Integer > 0).

HID         Harmonic identification number (Integer >= 0).

C           Component number (1 <= Integer <= 6).

A           Coefficient (Real; the first A must be nonzero).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. The first coordinate in the sequence is assumed to be the dependent
   coordinate and must be unique for all equations of the set.

3. Multipoint constraint sets must be selected in the Case Control Deck (MPC =
   SID) to be used by NASTRAN.

4. Dependent coordinates appearing on MPCAX cards may not appear on OMITAX,
   SPCAX, or SUPAX cards.

5. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

6. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


MPCS - Substructure Multipoint Constraints
==========================================

## Description

Defines multipoint constraints within or between substructures.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MPCS    |  SID  | NAME1 |   G1  |   C1  |   A1  |       |       |       |abc  |
|+bc     |       | NAME2 |  G21  |  C21  |  A21  |  G22  |  C22  |  A32  |def  |
|+ef     |       | NAME3 |  G31  |  C31  |  A31  |  G32  |  C32  |  A32  |ghi  |
|MPCS    |  171  |WINGRT |  966  |   1   |  1.0  |       |       |       |ABC  |
|+BC     |       |FUSELAG| 1036  |   1   | .031  | 1036  |   6   | 32.7  |DEF  |
|+EF     |       | CABIN |   39  |   2   | .076  |       |       |       |     |

Field       Contents

SID         Set identification number (Integer > 0).

NAMEi       Basic substructure name (BCD).

Gi          Grid or scalar point identification number in basic substructure
            NAME or NAMEi (Integer > 0).

Ci          Component number - Any one of the digits 1 - 6 in the case of
            geometric gridpoints; blank or zero in the case of scalar points
            (Integer > 0).

Ai          Coefficient (Real; A must be non-zero).

## Remarks

1. The first degree of freedom in the sequence is the dependent degree of
   freedom and it must be unique for all equations of the set.

2. MPCS constraints may be imposed only at the SOLVE step of substructuring in
   Phase 2. Therefore, referenced grid point components must exist in the
   final solution substructure.

3. The operation will constrain the degrees of freedom by the equation:

   - A  u   =  0
      i  i

   where ui is the displacement defined by NAMEi, Gi, and Ci.

4. Components may be connected within substructures and/or to separate
   substructures.

5. The dependent degree of freedom may not also be referenced on any SPCS,
   SPCS1, SPCSD, SPC, SPC1, OMIT, OMIT1, or SUPORT cards.

6. Multipoint constraint sets must be selected in the Case Control Deck (MPC =
   SID) to be used by NASTRAN.

7. MPCS cards may be referenced by an MPCADD card.


MTTPZ1 - Piezoelectric Material Temperature Dependence
======================================================

## Description

Specifies table references for piezoelectric material properties on a MATPZ1
card that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MTTPZ1  |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+a   |
|+a      |   R8  |   R9  |  R10  |  R11  |  R12  |  R13  |  R14  |       |     |
|MTTPZ1  |  703  |  201  |  202  |  203  |  204  |  205  |  206  |  207  |+A   |
|+A      |  208  |  209  |  210  |  211  |  212  |  213  |  214  |       |     |

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MATPZ1 card (Integer > 0).

Ri          References to table identification numbers for the corresponding
            fields on the MATPZ1 card (Integer > 0 or blank).

## Remarks

1. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MATPZ1 card, and the quantity remains constant.

2. TABLEM1, TABLEM2, TABLEM3, and TABLEM4 type tables may be used.

3. Material properties given on the basic MATPZ1 card are initial values. If
   two or more quantities are to retain a fixed relationship, then two or more
   tables must be input to define the relationship.


MTTPZ2 - Piezoelectric Material Temperature Dependence
======================================================
## Description

Specifies table references for piezoelectric material properties on a MATPZ2
card that are temperature-dependent.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|MTTPZ2  |  MID  |   R1  |   R2  |   R3  |   R4  |   R5  |   R6  |   R7  |+a   |
|MTTPZ2  |   35  |  701  |  702  |  703  |  704  |  705  |  706  |  707  |+A   |
        .                                      .
        .                                      .
        .                                      .
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+f      |  R48  |  R49  |  R50  |  R51  |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+F      |  748  |  749  |  750  |  751  |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field       Contents

MID         Material property identification number which matches the
            identification number on some basic MATPZ2 card (Integer > 0).

Ri          References to table identification numbers for the corresponding
            fields on the MATPZ2 card (Integer > 0 or blank).

## Remarks

1. Blank or zero entries mean no table dependence of the referenced quantity
   on the basic MATPZ2 card, and the quantity remains constant.

2. TABLEM1, TABLEM2, TABLEM3, and TABLEM4 type tables may be used.

3. Material properties given on the basic MATPZ2 card are initial values. If
   two or more quantities are to retain a fixed relationship, then two or more
   tables must be input to define the relationship.


NFTUBE - Nonlinear Transient Response Load
==========================================

## Description

Defines a nonlinear transient element for heat convection.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NFTUBE  |  CID  |   G1  |   G2  |   CP  | VOLRT |       |       |       |     |
|NFTUBE  |   20  |   8   |   12  |  1.3  |  8.0  |       |       |       |     |

Field       Contents
-----       --------
SID         Nonlinear load set identification number (Integer > 0).

G1, G2      Grid point identification numbers of connection points (Integer >
            0, G1 not equal G2).

CP          Heat capacity per unit volume (pCp) (Real).

VOLRT       Volume flow rate,  (Real or Integer). If real, the value is used;
            if integer is given, it is the ID of a TABLEDi data card.

## Remarks

1. Nonlinear loads are used only in transient analysis.

2. The power into grid points G1 and G2 is given by:

            .          .
   N1 = -pc v(t) U1    v > 0
            .
   N2 = +pc v(t) U1

   or

            .          .
   N1 = -pc v(t) U2    v > 0
            .
   N2 =  pc v(t) U2

3. This element does not contribute to the heat capacity matrix. The FTUBE
   element may be used for this purpose.

4. It is your responsibility to ensure flow continuity. There must be no
   accumulation of fluid mass at any grid point.


NOLIN1 - Nonlinear Transient Response Dynamic Load
==================================================

## Description

Defines nonlinear transient forcing functions of the form

   P (t)  =  S T(x (t))   ,
    i             j

where xj is either a displacement (uj) or a velocity (j).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN1  |  SID  |   GI  |   CI  |   S   |   GJ  |   CJ  |   T   |       |     |
|NOLIN1  |   21  |   3   |   4   |  2.1  |   3   |   1   |   6   |       |     |

Field       Contents

SID         Nonlinear load set identification number (Integer > 0).

GI          Grid or scalar or extra point identification number at which
            nonlinear load is to be applied (Integer > 0).

CI          Component number if GI is a grid point (0 < Integer <= 6); blank
            or zero if GI is a scalar or extra point.

S           Scale factor (Real).

GJ          Grid or scalar or extra point identification number (Integer > 0).

CJ          Component number if GJ is a grid point (0 < Integer <= 6; 11 <=
            Integer <= 16); blank or zero or 10 if GJ is a scalar or extra
            point (See Remark 4 below).

T           Identification number of a TABLEDi card (Integer > 0).

## Remarks

1. Nonlinear loads must be selected in the Case Control Deck (NONLINEAR = SID)
   to be used by NASTRAN.

2. Nonlinear loads may not be referenced on a DLOAD card.

3. All coordinates referenced on NOLIN1 cards must be members of the solution
   set. This means the ue set for modal formulation and the ud = ue + ua set
   for direct formulation.

4. The permissible values for the component number CJ are given in the
   following table:

+--------\--------------+---------------------+----------------------+
|xj        \          GJ|   Grid point        | Scalar or extra point|
+------------\----------+---------------------+----------------------+
| Displacement (u )     |  1 <= Integer <= 6  |    0 or blank        |
|                j      |                     |                      |
+-----------------------+---------------------+----------------------+
|           .           |                     |                      |
| Velocity (u )         | 11 <= Integer <= 16 |          10          |
|            j          |                     |                      |
+-----------------------+---------------------+----------------------+

   Note that velocity components are represented by integers ten greater than
   the corresponding displacement components.

5. If xj is a velocity (j), then it is determined from the relation

           u    -  u
   .        j,t     j,t-1
   u    =  ---------------
    j,t        ët

   where ët is the time increment and uj,t and uj,t-1 are the displacements at
   time t and at the previous time step respectively.

NOLIN2 - Nonlinear Transient Response Dynamic Load
==================================================

## Description

Defines nonlinear transient forcing functions of the form

   P (t)  =  S x (t)y (t)
    i           j    k

where xj and yk are either displacements (uj,uk) or velocities (j,k).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN2  |  SID  |   GI  |   CI  |   S   |   GJ  |   CJ  |   GK  |   CK  |     |
|NOLIN2  |   14  |   2   |   1   |  2.9  |   2   |   1   |   2   |   11  |     |

Field       Contents

SID         Nonlinear load set identification number (Integer > 0).

GI          Grid or scalar or extra point identification number at which
            nonlinear load is to be applied (Integer > 0).

CI          Component number if GI is a grid point (0 < Integer <= 6); blank
            or zero if GI is a scalar or extra point.

S           Scale factor (Real).

GJ          Grid or scalar or extra point identification number (Integer > 0).

CJ          Component number if GJ is a grid point (0 < Integer <= 6; 11 <=
            Integer <= 16); blank or zero or 10 if GJ is a scalar or extra
            point (See Remark 4 below).

GK          Grid or scalar or extra point identification number (Integer > 0).

CK          Component number if GK is a grid point (0 < Integer <= 6; 11 <=
            Integer < 16); blank or zero or 10 if GK is a scalar or extra
            point (See Remark 4 below).

## Remarks

1. Nonlinear loads must be selected in the Case Control Deck (NONLINEAR = SID)
   to be used by NASTRAN.

2. Nonlinear loads may not be referenced on a DLOAD card.

3. All coordinates referenced on NOLIN2 cards must be members of the solution
   set. This means the ue set for modal formulation and the ud = ue + ua set
   for direct formulation.

4. The permissible values for the component number CJ or CK are given in the
   following table:

+--------\--------------+---------------------+----------------------+
|xj or yk  \  GJ or GK  |   Grid point        | Scalar or extra point|
+------------\----------+---------------------+----------------------+
|Displacement (u  or u )|  1 <= Integer <= 6  |    0 or blank        |
|               j     k |                     |                      |
+-----------------------+---------------------+----------------------+
|           .     .     |                     |                      |
| Velocity (u  or u )   | 11 <= Integer <= 16 |          10          |
|            j     k    |                     |                      |
+-----------------------+---------------------+----------------------+

   Note that velocity components are represented by integers ten greater than
   the corresponding displacement components.

5. If xj or yk is a velocity (j or k), then it is determined from the
   relation

           u    - u                          u    -  u
   .       j,t     j,t-1            .         k,t     k,t-1
   u    = -----------------    or   u     = -----------------
    j,t        ët                    k,t           ët

   where ët is the time increment, uj,t and uk,t are the displacements at the
   time t and uj,t-1 and uk,t-1 are the displacements at the previous time step.

6. xj and yk need not both represent displacements or velocities. One of them
   may be a displacement and the other may be a velocity.

NOLIN3 - Nonlinear Transient Response Dynamic Load
==================================================

## Description

Defines nonlinear transient forcing functions of the form

                    A
            S(x (t)) ,  x (t) > 0
               j         j
   P (t) =
    i           0    ,  x (t) <= 0
                         j

where xj is either a displacement (uj) or a velocity (j).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN3  |  SID  |   GI  |   CI  |   S   |   GJ  |   CJ  |   A   |       |     |
|NOLIN3  |   4   |   102 |       | -6.1  |   2   |   5   |  -3.5 |       |     |

Field       Contents

SID         Nonlinear load set identification number (Integer > 0).

GI          Grid or scalar or extra point identification number at which
            nonlinear load is to be applied (Integer > 0).

CI          Component number if GI is a grid point (0 < Integer <= 6); blank
            or zero if GI is a scalar or extra point.

S           Scale factor (Real).

GJ          Grid or scalar or extra point identification number (Integer > 0).

CJ          Component number if GJ is a grid point (0 < Integer <= 6; 11 <=
            Integer <= 16); blank or zero or 10 if GJ is a scalar or extra
            point (See Remark 4 below).

A           Amplification factor (Real).

## Remarks

1. Nonlinear loads must be selected in the Case Control Deck (NONLINEAR = SID)
   to be used by NASTRAN.

2. Nonlinear loads may not be referenced on a DLOAD card.

3. All coordinates referenced on NOLIN3 cards must be members of the solution
   set. This means the ue set for modal formulation and the ud = ue + ua set
   for direct formulation.

4. The permissible values for the component number CJ are given in the
   following table:

+--------\--------------+---------------------+----------------------+
|xj        \          GJ|   Grid point        | Scalar or extra point|
+------------\----------+---------------------+----------------------+
| Displacement (u )     |  1 <= Integer <= 6  |    0 or blank        |
|                j      |                     |                      |
+-----------------------+---------------------+----------------------+
|           .           |                     |                      |
| Velocity (u )         | 11 <= Integer <= 16 |          10          |
|            j          |                     |                      |
+-----------------------+---------------------+----------------------+

   Note that velocity components are represented by integers ten greater than
   the corresponding displacement components.

5. If xj is a velocity (j), then it is determined from the relation

            u     -  u
   .         j,t      j,t-1
   u    =  -----------------
    j,t           ët

   where ët is the time increment and uj,t and uj,t-1 are the displacements at
   time t and at the previous time step, respectively.

NOLIN4 - Linear Transient Response Dynamic Load
===============================================

## Description

Defines nonlinear transient forcing functions of the form

                      A
            -S(-x (t)) ,  x (t) < 0
                 j         j
   P (t) =
    i             0    ,  x (t) >= 0
                           j

where xj is either a displacement (uj) or a velocity (j).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN4  |  SID  |   GI  |   CI  |   S   |   GJ  |   CJ  |   A   |       |     |
|NOLIN4  |   2   |   4   |   6   |  2.0  |  101  |       |  16.3 |       |     |

Field       Contents

SID         Nonlinear load set identification number (Integer > 0).

GI          Grid or scalar or extra point identification number at which
            nonlinear load is to be applied (Integer > 0).

CI          Component number if GI is a grid point (0 < Integer <= 6); blank
            or zero if GI is a scalar or extra point.

S           Scale factor (Real).

GJ          Grid or scalar or extra point identification number (Integer > 0).

CJ          Component number if GJ is a grid point (0 < Integer <= 6; 11 <=
            Integer <= 16); blank or zero or 10 if GJ is a scalar or extra
            point (See Remark 4 below).

A           Amplification factor (Real).

## Remarks

1. Nonlinear loads must be selected in the Case Control Deck (NONLINEAR = SID)
   to be used by NASTRAN.

2. Nonlinear loads may not be referenced on a DLOAD card.

3. All coordinates referenced on NOLIN4 cards must be members of the solution
   set. This means the ue set for modal formulation and the ud = ue + ua set
   for direct formulation.

4. The permissible values for the component number CJ are given in the
   following table:

+--------\--------------+---------------------+----------------------+
|xj        \          GJ|   Grid point        | Scalar or extra point|
+------------\----------+---------------------+----------------------+
| Displacement (u )     |  1 <= Integer <= 6  |    0 or blank        |
|                j      |                     |                      |
+-----------------------+---------------------+----------------------+
|           .           |                     |                      |
| Velocity (u )         | 11 <= Integer <= 16 |          10          |
|            j          |                     |                      |
+-----------------------+---------------------+----------------------+

   Note that velocity components are represented by integers ten greater than
   the corresponding displacement components.

5. If xj is a velocity (j), then it is determined from the relation

            u     -  u
   .         j,t      j,t-1
   u    =  -----------------
    j,t           ët

   where ët is the time increment and uj,t and uj,t-1 are the displacements at
   time t and at the previous time step, respectively.

NOLIN5 - Nonlinear Transient Load for Radiant Heat Transfer
===========================================================

## Description

Defines nonlinear transient radiant heat transfer with temperature dependent
emissivities and absorptivities.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN5  |  SID  |   AA  |   AB  |  FAB  |   EA  |   EB  |  ALPA |  ALPB |abc  |
|+bc     |  GA1  |  GA2  |  GA3  |  GA4  |  GB1  |  GB2  |  GB3  |  GB4  |     |
|NOLIN5  |   6   |  6.5  |  8.3  |  6.5  |   66  |  .83  |   77  |  .88  |ABC  |
|+BC     |   7   |  12   |  13   |       |  16   |  18   |       |       |     |

Field       Contents

SID         Nonlinear set identification number (Integer > 0).

AA, AB      Areas of elements A,B (Real > 0.0).

FAB         Exchange coefficient between areas A,B (Real > 0.0).

EA, EB      Values for emissivities of elements A,B (Real > 0.0, or Integer >
            0 if a table ID).

ALPA, ALPB  Values for absorptivities of elements A,B (Real > 0.0, or Integer
            > 0 if a table ID).

GA1,...,GA4 Grid points associated with Area A (Integer or blank) (GA1 > 0).

GB1,...,GB4 Grid points associated with Area B (Integer or blank) (GB1 > 0).

## Remarks

1. This card describes the radiant exchange between two areas, A and B. From
   zero through four grid points can be associated with each area.

2. All grid points specified for an area are treated equally.

3. The nonlinear loads for areas A and B are given by:

                         4
   N            (u +T   )
    A             A  abs
        = [R]            4
   N            (u +T   )
    B             B  abs

   The exchange matrix R depends upon AA, AB, FAB, EA, EB, ALPA, ALPB and the
   Stefan Boltzman constant. See Theoretical Manual Section 8.3.4 for the
   formula.

4. The second continuation card is not required. The default gives the
   absorptivity equal to the emissivity.

5. All grid points listed must be in the solution set {ud}.

6. Fields 6 through 9 may contain either a real value if a constraint
   emissivity or absorptivity is desired, or an integer value for the ID of a
   TABLEDi data card for temperature-dependent parameters.


NOLIN6 - Nonlinear Transient Response Dynamic Load
==================================================

## Description

Defines nonlinear transient forcing functions of the form

   P (t) = S T(x (t)) | x (t) | x (t),     if CJ <= 6
    i           j     |  j    |  j

                        .       .
   P (t) = S T(x (t)) | x (t) | x (t),     if CJ >= 10
    i           j     |  j    |  j

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|NOLIN6  |  SID  |   GI  |   CI  |   S   |   GJ  |   CJ  |   T   |       |     |
|NOLIN6  |   21  |   3   |   4   |  2.1  |   3   |   1   |   6   |       |     |

Field       Contents

SID         Nonlinear load set identification number (Integer > 0).

GI          Grid or scalar or extra point identification number at which
            nonlinear load is to be applied (Integer > 0).

CI          Component number if GI is a grid point (0 < Integer <= 6); blank
            or zero if GI is a scalar or extra point.

S           Scale factor (Real).

GJ          Grid or scalar or extra point identification number (Integer > 0).

CJ          Component number if GJ is a grid point (0 < Integer <= 6; 11 <=
            Integer <= 16); blank or zero or 10 if GJ is a scalar or extra
            point (See Remark 4 below).

T           Identification number of a TABLEDi card (Integer > 0).

## Remarks

1. Nonlinear loads must be selected in the Case Control Deck (NONLINEAR = SID)
   to be used by NASTRAN.

2. Nonlinear loads may not be referenced on a DLOAD card.

3. All coordinates referenced on NOLIN6 cards must be members of the solution
   set. This means the ue set for modal formulation and the ud = ue +  ua set
   for direct formulation.

4. The permissible values for the component number CJ are given in the
   following table:

+------\----------------+---------------------+----------------------+
| .      \              |                     |                      |
| x  or x  \        CJ  |   Grid Point        | Scalar or extra point|
|  j     j   \          |   Component         |                      |
+--------------\--------+---------------------+----------------------+
| Displacement (x )     |  1 <= Integer <= 6  |    0 or blank        |
|                j      |                     |                      |
+-----------------------+---------------------+----------------------+
|           .           |                     |                      |
| Velocity (x )         | 11 <= Integer <= 16 |          10          |
|            j          |                     |                      |
+-----------------------+---------------------+----------------------+

   Note that velocity components are represented by integers ten greater than
   the corresponding displacement components.

5. Velocity (j) is determined from the relation

            x      x
     .       j,t -  j,t-1
     x    =  _________________   ,
      j,t         ët

   where ët is the time increment and xj,t and xj,t-1 are the displacements at
   time t and at the previous time step respectively.

6. Since the forcing function Pi(t) is a product of TABLEDi, displacement,
   velocity and the scale factor S, any zero value of these quantities will
   make Pi(t) equal to zero. This condition may occur when the initial
   displacements or velocities are zero, and no other load is applied to the
   structure.


OMIT - Omitted Coordinates
==========================

## Description

Defines coordinates (degrees of freedom) to be omitted from the problem
through matrix partitioning. Used to reduce the number of independent degrees
of freedom.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|OMIT    |   ID  |   C   |   ID  |   C   |   ID  |   C   |   ID  |   C   |     |
|OMIT    |   16  |   2   |   23  | 3516  |       |       |   1   |   4   |     |

Field       Contents

ID          Grid or scalar point identification number (Integer > 0).

C           Component number, zero, or blank for scalar points, any unique
            combination of the digits 1 - 6 for grid points.

## Remarks

1. Coordinates specified on OMIT cards may not be specified on OMIT1, ASET,
   ASET1, SUPORT, SPC, or SPC1 cards nor may they appear as dependent
   coordinates in multipoint constraint relations (MPC) or in rigid elements
   (RIGD1, RIGD2, RIGD3, or RIGDR) or as permanent single-point constraints on
   GRID cards.

2. As many as 24 coordinates may be omitted by a single card.

3. ASET or OMIT data are not recommended for use in heat transfer analysis
   with radiation effects.

OMIT1 - Omitted Coordinates
===========================
## Description

Defines coordinates (degrees of freedom) to be omitted from the problem
through matrix partitioning. Used to reduce the number of independent degrees
of freedom.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|OMIT1   |   C   |   G   |   G   |   G   |   G   |   G   |   G   |   G   |abc  |
|+bc     |   G   |   G   |   G   |  etc. |       |       |       |       |     |
|OMIT1   |   3   |   2   |   1   |   3   |  10   |   9   |   6   |   5   |ABC  |
|+BC     |   7   |   8   |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|OMIT1   |   C   |  ID1  |"THRU" |  ID2  |       |       |       |       |     |
|OMIT1   |   0   |   17  | THRU  |  109  |       |       |       |       |     |

Field       Contents

C           Component number (any unique combination of the digits 1 - 6 with
            no imbedded blanks when point identification numbers are grid
            points; must be null or zero if point identification numbers are
            scalar points).

G, ID1, ID2 Grid or scalar point identification number (Integer > 0; ID1 <
            ID2).

## Remarks

1. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multi-point constraint relation (MPC card) or as a degree
   of freedom on a rigid element (CRIGD1, CRIGD2, CRIGD3, CRIGDR), nor may it
   be referenced on a SPC, SPC1, OMIT, ASET, ASET1, or SUPORT card or on a
   GRID card as permanent single-point constraints.

2. If the alternate form is used, all of the grid (or scalar) points ID1
   through ID2 are assumed.

3. ASET or OMIT data are not recommended for use in heat transfer analysis
   with radiation effects.

OMITAX - Axisymmetric Omitted Coordinate
========================================

## Description

Defines coordinates to be omitted from a model containing CCONEAX, CTRAPAX, or
CTRIAAX elements through matrix partitioning. Used to reduce the number of
independent degrees of freedom.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|OMITAX  |  RID  |  HID  |   C   |  RID  |  HID  |   C   |       |       |     |
|OMITAX  |   2   |   6   |   3   |   4   |   7   |   1   |       |       |     |

Field       Contents

RID         Ring identification number (Integer > 0).

HID         Harmonic identification number (Integer > 0).

C           Component number (any unique combination of the digits 1 - 6).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. Up to 12 coordinates may be omitted via this card.

3. Coordinates appearing on OMITAX cards may not appear on MPCAX, SUPAX, or
   SPCAX cards.

4. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

5. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


PAERO1 - Aerodynamic Panel Property
===================================

## Description

Gives associated bodies for the panels in the Doublet-Lattice method.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PAERO1  |  PID  |  B1   |  B2   |  B3   |  B4   |  B5   |  B6   |       |     |
|PAERO1  |   1   |  3    |       |       |       |       |       |       |     |

Field       Contents

PID         Property identification number (referenced by CAERO1) (Integer >
            0).

B1,...,B6   ID of associated body (Integer >= 0 or blank).

## Remarks

1. The associated body must be in the same aerodynamic group (IGID).

2. If there are no bodies, the card is still required.

3. The Bi numbers above must appear on a PAERO2 card to define these bodies
   completely.


PAERO2 - Aerodynamic Body Properties
====================================

## Description

Defines the cross-section properties of aerodynamic bodies.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PAERO2  |  PID  |ORIENT | WIDTH |  AR   |  LRSB |  LRIB |  LTH1 |  LTH2 |ABC  |
|+BC     |  THI1 |  THN1 |  THI2 |  THN2 |  THI3 |  THN3 |  etc. |       |     |
|PAERO2  |   2   |   Z   |  6.0  |  1.0  |  22   |   91  |  100  |       |abc  |
|+bc     |   1   |   3   |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

ORIENT    Orientation flag Z, Y, or ZY. Type of motion allowed for bodies
          (BCD). Refers to the aerodynamic coordinate system y direction of
          ACSID (see AERO data card).

WIDTH     Reference half-width of body (Real > 0.).

AR        Aspect ratio (height/width) (Real > 0.).

LRSB      ID of an AEFACT data card containing a list of slender body
          half-widths. If blank, the value of WIDTH will be used (Integer >=
          0 or blank).

LRIB      ID of an AEFACT data card containing a list of interference body
          half-widths. If blank, the value of WIDTH will be used (Integer >=
          0 or blank).

LTH1, LTH2ID of AEFACT data cards for defining theta arrays for interference
          calculations (Integer >= 0 or blank).

THIi, THNiThe first and last interference element of a body to use the éi
          array (Integer >= 0).

## Remarks

1.The EID of all CAERO2 elements in any IGID group must be ordered, so that
  their corresponding ORIENT values appear in the order Z, ZY, Y.

2.The half-widths (given on AEFACT data cards referenced in field 6 and 7)
  are specified at division points. The number of entries on an AEFACT data
  card used to specify half-widths must be one greater than the number of
  elements.

3.The half-width at the first point (that is, the nose) on a slender body is
  usually 0.; thus it is recommended (but not required) that the LRSB data is
  supplied with a zero first entry.

4.THIi and THNi are interference element locations on a body. The first
  element is one for each body.

5.A body is represented by a slender body surrounded by an interference
  body.The slender body creates the downwash due to the motion of the body,
  while the interference body represents the effects upon panels and other
  bodies. The cross-section is elliptical.

                            z
                            | +---+---+---+---+---+---+
             Slender Body     +---+---+---+---+---+---+--- x
       (six elements shown)   +---+---+---+---+---+---+
                                      +-- Division Points
                              +-------+-------+-------+
        Interference Body     |       |       |       |--- x
     (three elements shown)   +-------+-------+-------+


                                  z
                                  |
                                  +------+ half width
                                  |
                                  |
                            é3 O  |  O é2       .
                                  |        .
         End View       é4 O      |    . O é1         Theta array, receiving
    (looking forward)             | .  é              points for interference
                                  +------------ y     body elements

                        é5 O             O é8

                            é6 O     O é7

                         Figure 2.4-43. PAERO2 diagram


PAERO3 - Aerodynamic Mach Box Surface Properties
================================================

## Description

Defines the number of Mach boxes in the flow direction and the location of
cranks and control surfaces of a Mach box lifting surface.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PAERO3  |  PID  | NBOX  | NCTRL |       |  X5   |  Y5   |  X6   |  Y6   |ABC  |
|+BC     |  X7   |  Y7   |  X8   |  Y8   |  X9   |  Y9   |  X10  |  Y10  |DEF  |
|+EF     |  X11  |  Y11  |  X12  |  Y12  |       |       |       |       |     |
|PAERO3  | 2001  |  15   |   2   |       |  0.   |  65.  |       |       |abc  |
|+bc     |  78.  |  65.  |  108. |  65.  |  82.  | 97.5  |  112. |  97.5 |def  |
|+ef     |  86.  |  130. |  116. |  130. |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

NBOX      The number of Mach boxes in flow direction (0 < Integer < 50).

NCTRL     Number of control surfaces (Integer 0, 1, or 2).

X5-Y12    Location of points 5 through 12, which are in the element
          coordinate system, to define the cranks and control surface
          geometry (Real).

## Remarks

1.The geometry is shown in Figure 2.4-1 at the CAERO3 Bulk Data card
  description.

2.If Y5 <= 0.0, there is no leading edge crank. Also, if Y6 <= 0.0, there is
  no trailing edge crank.

3.If NCTRL = 0, no continuation cards are needed. If NCTRL = 1 or 2, then
  NCTRL continuation cards are needed.

4.The relations Y7 >= Y8, Y9 >= Y10, and Y11 >= Y12 must hold.


PAERO4 - Aerodynamic Supersonic Strip Properties
================================================

## Description

Gives properties of each strip element for the strip theory.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PAERO4  |  PID  |  CLA  |  LCLA |  CIRC | LCIRC | DOC1  | CAOC1 |GAPOC1 |ABC  |
|+BC     |  DOC2 | CAOC2 |GAPOC2 | DOC3  | CAOC3 |GAPOC3 |. . . .| etc.  |. .  |
|PAERO4  | 6001  |   1   |  501  |   0   |   0   |  0.0  |  0.0  | 0.0   |abc  |
|+bc     |  0.50 | 0.25  | 0.02  | 0.53  | 0.24  | 0.0   |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

CLA       Parameter to select Prandtl-Glauert correction (Integer -1, 0, 1,
          or blank).

          -1         compressibility correction made to lift curve slope
                     data for a reference Mach number.

          0 or blank no correction and no list needed.

          +1         no correction and lift curve slope provided by a list
                     as a function of strip location and Mach number.

LCLA      ID number of AEFACT data card which lists the lift curve slope on
          all strips for each Mach number on MKAEROi data card (Integer = 0
          if CLA = 0, > 0 if CLA not equal 0) (see Remark 7(b) below).

CIRC      Parameter to select Theodorsen's function, C(k), or the number of
          exponential coefficients used to approximate C(k) (Integer 0, 1,
          2, 3, or blank. Must be zero if CLA not equal 0.)

          0 or blank Theodorsen function.

          1,2,3      approximate function with b0, b1, +1, ... bn, +n , n =
                     1,2,3.

LCIRC     ID number of AEFACT data card which lists the b, + values for each
          Mach number on the MKAEROi data card (Integer = 0 if CIRC = 0, > 0
          if CIRC not equal 0) (see Remarks 7(c), 7(d), and 7(e) below;
          variable b's and +'s for each m).

DOCi      d/c = distance of control surface hinge aft of quarter-chord
          divided by the strip chord (Real >= 0.0).

CAOCi     ca/c = control surface chord divided by strip chord (Real >= 0.0).

GAPOCi    g/c = control surface gap divided by strip chord (Real >= 0.0).

## Remarks

1.This card is required for strip theory with three entries (DOCi, CAOCi,
  GAPOCi) per strip.

2.If CLA = -1, lift curve slope data at one Mach number are needed on the
  AEFACT data card.

3.If CAOCi = 0.0, there is no control surface.

4.If GAPOCi = 0.0, there is slot flow.

5.If GAPOCi < 0.01, then 0.01 is used.

6.Imbedded blank fields are not allowed.

7.The following table lists the lift curve slope or lag function selection
  and the AEFACT data card formats used for strip theory.

+----------+--------------------+-------------------+-------------------+------+
|          |                    |Parameter          |                   |      |
|Theodorsen|       Data         |Combinations       |                   |Card  |
|Function  |       Type         +---+----+----+-----+       Number of   |Format|
|          |       Input        |CLA|LCLA|CIRC|LCIRC|       Words       |Index |
+----------+--------------------+---+----+----+-----+-------------------+------+
|Exact     |Lift Curve  c   =2+ | 0 |  0 | 0  |  0  | No AEFACT card required  |
|          |  Slope      l      |   |    |    |     |                   |      |
|          |              +i    |   |    |    |     |                   |      |
|          |                    |   |    |    |     |                   |      |
|          |c   input, uses     |-1 | ID | 0  |  0  |  (NSTRIP+1)       |  (a) |
|          | l   Prandtl-Glauert|   |    |    |     |                   |      |
|          |  +i  Correction    |   |    |    |     |                   |      |
|          |                    |   |    |    |     |                   |      |
|          |c  input,for all m's| 1 | ID | 0  |  0  |  (NSTRIP+1)*NMACH |  (b) |
|          | l   on MKAERO card |   |    |    |     |                   |      |
|          |  +i                |   |    |    |     |                   |      |
|          |                    |   |    |    |     |                   |      |
|Approxi-  | Coefficients -     | 0 |  0 | 1  |  ID |  4*NMACH          |  (c) |
|mate      |  b0i,b1i, +1i, etc.|   |    |    |     |                   |      |
|          |                    | 0 |  0 | 2  |  ID |  6*NMACH          |  (d) |
|          |                    |   |    |    |     |                   |      |
|          |                    | 0 |  0 | 3  |  ID |  8*NMACH          |  (e) |
+----------+--------------------+---+----+----+-----+-------------------+------+

  Card Format

  (a)  AEFACT, ID, m , c   , c   ,...,c
                    1   l     l        l
                         +     +        +
                          1     2        NSTRIP

  (b)  AEFACT, ID, m ,c    ,c    ,...,c         ,m ,c    ,c    ,...,c         ,
                    1  l     l         l          2  l     l         l
                        +     +         +             +     +         +
                         11    21        NSTRIP1       12    22        NSTRIP2
       etc., for all m on MKAEROi data card.

  (c)  AEFACT, ID, m , b  , b  , +  , m , b  , b  , +  , m , etc.
                    1   01   11   11   2   02   12   12   3

  (d)  AEFACT, ID, m , b  , b  , +  , b  , +  ,m , etc.
                    1   01   11   11   21   21  2

  (e)  AEFACT, ID, m , b  , b  , +  , b  , +  , b  , +  , m , etc.
                    1   01   11   11   21   21   31   31   2


PAERO5 - Aerodynamic Strip Element Properties
=============================================

## Description

Gives properties of each strip element for piston theory.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PAERO5  |  PID  |NALPHA |LALPHA |  NXIS |  NXIS | NTAUS | LTAUS |       |ABC  |
|+BC     | CAOC1 | CAOC2 | CAOC3 | CAOC4 | CAOC5 | etc.  |. . . .| . . . |     |
|PAERO5  | 7001  |   1   |  702  |   1   |  701  |  1    |  700  |       |abc  |
|+bc     |  0.0  | 0.0   | 5.25  |3.99375| 0.0   |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

NALPHA    Number of angles of attack (+) input per Mach number (m, of
          MKAEROi data card) (Integer > 0) (see Remark 3 below).

LALPHA    ID number of the required AEFACT data card which lists the +'s
          (Integer > 0).

NXIS      Number of dimensionless chordwise coordinates (î) used to define
          the geometry of the strips (Integer >= 0 or blank) (see Remark 4
          below).

LXIS      ID number of the AEFACT data card which lists the î`s (Integer = 0
          if Ca = 0, NTHICK > 0, Integer > 0 if Ca > 0, NTHICK = 0) where Ca
          is control surface chord length.

NTAUS     Number of thickness ratios (ç) used to define the geometry of the
          strips (Integer >= 0 or blank).

LTAUS     ID number of the AEFACT data card which lists the ç`s (Integer = 0
          or blank if NTAUS = 0, Integer > 0 if NTAUS > 0).

CAOCi     Ratio of chord of control surface to chord of strip (Ca/c) for
          each strip (Real >= 0).

## Remarks

1.A PAERO5 card is used for piston theory strip property definition and is
  referenced in the PID column of a CAERO5 card.

2.The continuation card is required. The number of entries must equal the
  number of strips (from CAERO5). Imbedded blank fields are forbidden, so use
  0.0 if there is no control surface.

3.The following table lists the formats of the AEFACT data cards for angle of
  attack distribution.

+-------------------------+----------+--------+
|                         |          | LALPHA |
|      TYPE OF DATA       |  NALPHA  | FORMAT |
+-------------------------+----------+--------+
|  Same + for all strips  |    1     |  (a)   |
|                         |          |        |
|  Variable +             |  NSTRIP  |  (b)   |
+-------------------------+----------+--------+

              Number
    Format   of Words

     (a)    2*NMACH     AEFACT, ID, m , + , m , + , ...
                                     1   1   2   2

     (b)    (l+NSTRIP)  AEFACT, ID, m , +  , +  , ..., +        , ...
            *NMACH                   1   11   21        NSTRIP,l
                         (repeat for all m's)

4.The following table lists the formats of the AEFACT data cards for
  thickness and other list data.

+--------------------------+------+------+------+--------+--------+--------+
|                          |      |NTHICK|      |  LXIS  |        |  LTAUS |
|   TYPE OF INPUT DATA     | CAOCi|FORMAT| NXIS | FORMAT | NTAUS  | FORMAT |
+--------------------------+------+------+------+--------+--------+--------+
|  Integrals are input     |      |      |      |        |        |        |
|  -------------------     |      |      |      |        |        |        |
|  Same for all strips,    |  0.  |  (c) |   0  |    0   |   0    |    0   |
|  no control surfaces     |      |      |      |        |        |        |
|                          |      |      |      |        |        |        |
|  Same for all strips     |.NE.0.|  (d) |   1  |   (e)  |   0    |    0   |
|  with control surfaces   |      |      |      |        |        |        |
|                          |      |      |      |        |        |        |
|  Separate hinge for      |.NE.0.|  (d) |NSTRIP|   (f)  |   0    |    0   |
|  each strip with         |      |      |      |        |        |        |
|  control surfaces        |      |      |      |        |        |        |
|                          |      |      |      |        |        |        |
|  Thickness data are input|      |      |      |        |        |        |
|  ------------------------+      |      |      |        |        |        |
|  Same for all strips,    |  0.  |   0  |   1  |   (g)  |    1   |  (h)   |
|  no control surfaces     |      |      |      |        |        |        |
|                          |      |      |      |        |        |        |
|  Same for all strips     |.NE.0.|   0  |   1  |   (g)  |    1   |  (h)   |
|  with control surfaces   |      |      |      |        |        |        |
|                          |      |      |      |        |        |        |
|  Separate data for       |.NE.0.|   0  |NSTRIP|   (i)  | NSTRIP |  (j)   |
|  each strip with         |      |      |      |        |        |        |
|  control surfaces        |      |      |      |        |        |        |
+--------------------------+------+------+------+--------+--------+--------+

              Number
    Format   of Words

     (c)        6            AEFACT, ID, I , I , I , I , I , I
                                          1   2   3   4   5   6

     (d)        12           AEFACT, ID, I , ..., I , J , ..., J
                                          1        6   1        6

     (e)        1            AEFACT, ID, î
                                          h

     (f)      NSTRlP         AEFACT, ID, î  , î  , ..., î
                                          h1   h2        h
                                                          NSTRIP

     (g)        2            AEFACT, ID, î , î
                                          m   h

     (h)        3            AEFACT, ID, ç , ç , ç
                                          m   h   t

     (i)     2*NSTRIP        AEFACT, ID, î  , î  , ..., î
                                          m1   h1        h
                                                          NSTRIP

     (j)     3*NSTRIP        AEFACT, ID, ç  , ç  , ç  , ..., ç
                                          m1   h1   t1        t
                                                               NSTRIP

    Note:   If there is no hinge, you may put î  = ç  = 0.
                                               h    h

Dimensions of symmetrical airfoil, internal integral calculation are shown in
Figure 2.4-44.

                  ç                             +----- ç
                  +      +--- g                 |       h
                  |      |     î                | +--- Hinge Line
                  |      |         .     .     .| |
     Flow         |      |   .           |ç     | |  .
     ---------+   |    .                 | m               .   |
                  +----------------------+------+-+-----------------+ î
                 -+ î  +-                |      | |        |   |
                  +--------î ------------+        |        |   ç
                  |         m            |        |        |    t
                  |                               |        |
                  +--------------î ---------------+        |
                  |               h                        |
                  +------------------î=1-------------------+

                         Figure 2.4-44. PAERO5 diagram


PARAM - Parameter
=================

## Description

Specifies values for parameters used in DMAP sequences (including rigid
formats).

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PARAM   |   N   |   V1  |   V2  |       |       |       |       |       |     |
|PARAM   | IRES  |   1   |       |       |       |       |       |       |     |

Field     Contents
-----     --------
N         Parameter name (one to eight alphanumeric characters, the first of
          which must be alphabetic).

V1, V2    Parameter value based on parameter type as follows:

|       Type                 |      V1           |       V2        |
|----------------------------|-------------------|-----------------|
|  Integer                   |  Integer          | Blank           |
|  Real, single-precision    |  Real             | Blank           |
|  BCD (alphanumeric)        |  BCD              | Blank           |
|  Real, double-precision    |  Double-precision | Blank           |
|  Complex, single-precision |  Real             | Real            |
|  Complex, double-precision |  Double-precision | Double-precision|

## Remarks

1.Only parameters for which assigned values are allowed may be given values
  via the PARAM card. Section 5 describes parameters as used in DMAP.

2.The following is a list of parameters, arranged in alphabetical order.

APRESS - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). A positive integer value causes the
generation of aerodynamic pressure loads. A negative integer value suppresses
the generation of these loads. The default value is -1.

ASETOUT - optional in all rigid formats. A positive integer value of this
parameter causes the ASET (or HASET) output data block to be generated by the
GP4 module. A negative integer value or 0 suppresses the generation of this
output data block. The default value is 0.

ATEMP - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). A positive integer value causes the
generation of aerodynamic temperature loads. A negative value suppresses the
generation of these loads. The default value is -1.

AUTOSPC - optional in all rigid formats. Gives you the option of
automatically applying single-point constraints for the purpose of removing
potential grid (and scalar) point singularities that have not been otherwise
already constrained out. The meanings of the various values for this parameter
are as follows.

AUTOSPC = 0

The AUTOSPC feature is not used. This is the default value.

AUTOSPC = 1

All singularities except those that are:

  removed via single-point constraints, or
  removed via multipoint constraints, or
  specified as independent degrees of freedom in multipoint
  constraints or rigid elements, or
  specified on SUPORT cards

are removed by the automatic application of single-point constraints.

A set of SPC1 cards is generated and printed for your information and
convenience, indicating the singularities that have been automatically removed
as above. These SPC1 cards have the same SPC set ID as the current subcase (or
an SPC set ID of 1 if the current subcase has no SPC set).

AUTOSPC = 2

There are two possible cases.

Case 1. There are no omitted degrees of freedom in the current subcase. This
case is handled in the same way as the AUTOSPC = 1 case. SPC1 cards are
generated and printed as in the AUTOSPC = 1 case.

Case 2. There are omitted degrees of freedom in the current subcase. This case
is handled in the same way as the AUTOSPC = 1 case, but with one important
difference, as follows.

All singularities except those that are:

  removed via single-point constraints, or
  removed via multipoint constraints, or
  specified as independent degrees of freedom in multipoint
  constraints or rigid elements, or
  specified on SUPORT cards

are removed by the automatic application of single-point constraints, but only
if the singularity is part of the o-set (omitted set).

SPC1 cards are generated and printed as in the AUTOSPC = 1 case.

AUTOSPC = -1

This case is handled in the same way as the AUTOSPC = 1 case, except that the
SPC1 cards generated are both punched and printed.

AUTOSPC = -2

This case is handled in the same way as the AUTOSPC = 2 case, except that the
SPC1 cards generated are both punched and printed.

AUTOSPC < -2 or > 2

These illegal values cause singularity processing to be skipped in the GP4
module, the same as if the value were 0.

BETA - optional in transient heat transfer analysis (HEAT rigid format 9).
The real value of this parameter is used as a factor in the integration
algorithm (see Section 8.4.2 of the Theoretical Manual). The default value is
0.55.

BETAD - optional in static analysis with differential stiffness and static
aerothermoelastic design/analysis of axial flow compressors (DISP rigid
formats 4 and 16). The integer value of this parameter is the number of
iterations allowed for computing the load correction in the inner (load) loop
before shifting to the outer (stiffness) loop, which adjusts the differential
stiffness. The default value is 4 iterations.

COUPMASS - optional in all DISP and AERO rigid formats. A positive integer
value of this parameter causes the generation of coupled mass matrices rather
than lumped mass matrices for all bar elements, rod elements, and plate
elements that include bending stiffness. This option applies to both
structural and nonstructural mass for the following elements: BAR, CONROD,
QUAD1, QUAD2, ROD, TRIA1, TRIA2 and TUBE. Since structural mass is not defined
for the following list of elements, the option applies only to the
nonstructural mass: QDPLT, TRBSC and TRPLT. A negative value causes the
generation of lumped mass matrices (translational components only) for all of
the above elements. (This is the default.) A zero value activates the
following parameters.

CPBAR, CPROD, CPQUAD1, CPQUAD2, CPTRIA1, CPTRIA2, CPTUBE, CPQDPLT, CPTRPLT, and
CPTRBSC - optional in all DISP and AERO rigid formats. These parameters are
active only if COUPMASS = 0. A positive value causes the generation of coupled
mass matrices for all elements of that particular type as shown by the
following table:

  Parameter       Element Types

  CPBAR      BAR
  CPROD      ROD, CONROD
  CPQUAD1       QUAD1
  CPQUAD2       QUAD2
  CPTRIA1       TRIA1
  CPTRIA2       TRIA2
  CPTUBE        TUBE
  CPQDPLT       QDPLT
  CPTRPLT       TRPLT
  CPTRBSC       TRBSC

A negative value (the default) for these parameters causes the generation of
the lumped mass matrices (translational components only) for these element
types.

CTYPE - required in rigid formats using the cyclic symmetry feature (DISP
rigid formats 14 and 15 and AERO rigid format 9). The BCD value of this
parameter defines the type of cyclic symmetry as follows:

  (1) ROT - rotational symmetry
  (2) DRL - dihedral symmetry, using right and left halves
  (3) DSA - dihedral symmetry, using symmetric and antisymmetric components.

CYCIO - optional in static analysis with cyclic symmetry (DISP rigid format
14). The integer value of this parameter specifies the form of the input and
output data. A value of +1 is used to specify physical segment representation,
and a value of -1 for cyclic transform representation. The default value is
+1.

CYCSEQ - optional in rigid formats using the cyclic symmetry feature (DISP
rigid formats 14 and 15 and AERO rigid format 9). The integer value of this
parameter specifies the procedure for sequencing the equations in the solution
set. A value of +1 specifies that all cosine terms should be sequenced before
all sine terms, and a value of -1 specifies alternating cosine and sine terms.
The default value is -1.

EPSHT - optional in nonlinear static heat transfer analysis (HEAT rigid
format 3). The real value of this parameter is used to test the convergence of
the nonlinear heat transfer solution (see Section 8.4.1 of the Theoretical
Manual). The default value is 0.001.

EPSIO - optional in static analysis with differential stiffness and static
aerothermoelastic design/analysis of axial flow compressors (DISP rigid
formats 4 and 16). The real value of this parameter is used to test the
convergence of the iterated differential stiffness. The default value is
10**(-5).

FXCOOR, FYCOOR, and FZCOOR - optional in static aerothermoelastic
design/analysis of axial flow compressors (DISP rigid format 16). The real
values of these parameters are the fractions of the displacements used to
redefine the blade geometry. The default values are:  FXCOOR = 1.0, FYCOOR =
1.0 and FZCOOR = 1.0.

G - optional in the direct formulation of all DISPLACEMENT dynamics problems
(DISP rigid formats 7, 8 and 9). The real value of this parameter is used as a
uniform structural damping coefficient in the direct formulation of dynamics
problems (see section 9.3.3 of the Theoretical Manual). Not recommended for
use in hydroelastic problems.

GRDEQ - optional in static and normal modes analyses (DISP rigid formats 1,
2, 3, 14, and 15). A positive integer value of this parameter selects the grid
point about which equilibrium will be checked for the Case Control output
request, MPCFORCE. If the integer value is zero, the basic origin is used. The
default value is -1.

GRDPNT - optional in all DISP and AERO rigid formats. A positive integer
value of this parameter causes the Grid Point Weight Generator to be executed.
The value of the integer indicates the grid point to be used as a reference
point. If the integer is zero (blank is not equivalent) or is not a defined
grid point, the reference point is taken as the origin of the basic coordinate
system. All fluid related masses are ignored. Additional details for the Grid
Point Weight Generator are given in Section 5.5 of the Theoretical Manual. The
following weight and balance information is automatically printed following
the execution of the Grid Point Weight Generator.

  (1) Reference point.
  (2) Rigid body mass matrix [MO] relative to the reference point in the
basic coordinate system.
  (3) Transformation matrix [S] from basic coordinate system to principal
mass axes.
  (4) Principal masses (mass) and associated centers of gravity (X-C.G.,
Y-C.G., Z-C.G.).
  (5) Inertia matrix I(S) about the center of gravity relative to the
principal mass axes.
  (6) Inertia matrix I(Q) about the center of gravity relative to the
principal inertia axes.
  (7) Transformation matrix [Q] between S-axes and Q-axes.

GUSTAERO - optional in AERO rigid formats 10 and 11. An integer value of +1
causes gust loads to be computed. The default value is -1 for no gust loads.

IFTM - optional in aeroelastic response (AERO rigid format 11). The integer
value of this parameter selects the method for the integration of the Inverse
Fourier Transform. An integer value of 0 specifies a rectangular fit; 1
specifies a trapezoidal fit; and 2 specifies a cubic spline fit to obtain
solutions versus time for which aerodynamic forces are functions of frequency.
The default value is 0.

INTERACT - optional in DISP static analysis (DISP rigid format 1). This
parameter, like the SYS21 parameter, is of relevance only when your primary
purpose is to make interactive restart runs. In such a case, the integer value
of this parameter must be set to -1 (via a PARAM bulk data card) in both the
batch checkpoint run (that precedes the interactive restart run) as well as in
the interactive restart run. If not so specified via a PARAM bulk data card,
the COMPOFF and COMPON instructions in the DMAP sequence that use this
parameter assume a value of 0 for this parameter (see Section 5.7).

IPRTCI, IPRTCL, and IPRTCF - optional in static aerothermoelastic 
design/analysis of axial flow compressors (DISP rigid format 16). If IPRTi is 
a positive integer, then intermediate print will be generated in the ALG 
module based on the print option in the ALGDB data table. If IPRTi = 0 (the 
default), no intermediate print will be generated. 

IREF - optional in blade cyclic modal flutter analysis (AERO rigid format 9).
A positive integer value of this parameter defines the reference streamline
number. IREF must be equal to an SLN on a STREAML2 bulk data card. The default
value of -1 represents the stream surface at the blade tip. If IREF does not
correspond to an SLN, then the default will be taken.

IRES - optional in all DISP and HEAT statics problems (DISP rigid formats 1,
2, 4, 5, 6, 14, and 16 and HEAT rigid formats 1 and 3). A positive integer
value of this parameter causes the printing of the residual vectors following
each execution of the SSG3 (or SSGHT) module.

ISTART - optional in direct and modal transient response (DISP rigid formats 9
and 12). A positive value of this parameter causes the second (or alternate)
starting method to be used (see Section 11.4 of the Theoretical Manual). The
alternate starting method is recommended when initial accelerations are
significant and when the mass matrix is non-singular. The default value is -1
and causes the first starting method to be used.

KDAMP - optional in all AERO rigid formats. An integer value of +1 causes
modal damping terms to be put into the complex stiffness matrix for structural
damping (+1 recommended for K and KE methods). The default value is -1.

KGGIN - optional in blade cyclic modal flutter analysis (AERO rigid format
9). A positive integer value of this parameter indicates that your stiffness
matrix is to be read from an external file (GINO file INPT) via the INPUTT1
module in the rigid format. The default value is -1 when not needed.

KINDEX - required in normal modes analysis with cyclic symmetry (DISP rigid
format 15) and in blade cyclic modal flutter analysis (AERO rigid format 9).
The integer value of this parameter specifies a single value of the harmonic
index. Higher KINDEX no. will result in getting higher mode.

KMAX - optional in static analysis with cyclic symmetry (DISP rigid format
14). The integer value of this parameter specifies the maximum value of the
harmonic index. The default value is ALL which implies NSEGS/2 for NSEGS even
and (NSEGS - 1)/2 for NSEGS odd.

KTOUT - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). A positive integer value of this parameter
indicates that you want to save the total stiffness matrix on an external file
(GINO file INPT) via the OUTPUT1 module in the rigid format. The default value
is -1 when not needed.

LFREQ and HFREQ - required in all modal formulations of DISP and AERO
dynamics problems (DISP rigid formats 10, 11 and 12 and AERO rigid formats 9,
10 and 11), unless LMODES is used. The real values of these parameters give
the cyclic frequency range (LFREQ is the lower limit and HFREQ is the upper
limit) of the modes to be used in the modal formulation. To use this option,
parameter LMODES must be set to 0.

LMODES - required in all modal formulations of DISP and AERO dynamics
problems (DISP rigid formats 10, 11 and 12 and AERO rigid formats 9, 10 and
11), unless parameters LFREQ and HFREQ are used. The integer value of this
parameter is the number of lowest modes to be used in the modal formulation.

LOCATION and INPTUNIT - required in static aerothermoelastic design/analysis
of axial flow compressors (DISP rigid format 16) when using the KTOUT
parameter, and in blade cyclic modal flutter analysis (AERO rigid format 9)
when using the KGGIN parameter. See Section 5.5 for a description of these
parameters which are required by the INPUTT1 and OUTPUT1 modules. The default
values for LOCATION and INPTUNIT are -1 and 0, respectively.

MACH - optional in AERO rigid formats 10 and 11. The real value of this
parameter selects the closest Mach numbers to be used to compute aerodynamic
matrices. The default value is 0.0.

MAXIT - optional in nonlinear static heat transfer analysis (HEAT rigid
format 3). The integer value of this parameter limits the maximum number of
iterations. The default value is 4 iterations.

MAXMACH - optional in blade cyclic modal flutter analysis (AERO rigid format
9). The real value of this parameter is the maximum Mach number below which
the subsonic unsteady cascade theory is valid. The default value is 0.80.

MINMACH - optional in blade cyclic modal flutter analysis (AERO rigid format
9). The real value of this parameter is the minimum Mach number above which
the supersonic unsteady cascade theory is valid. The default value is 1.01.

MODACC - optional in the modal formulation of frequency response (DISP rigid
format 11) and transient response (DISP rigid format 12) problems. A positive
integer value of this parameter causes the Dynamic Data Recovery module to use
the mode acceleration method. Not recommended for use in hydroelastic
problems. DMAP module GKAD sets the V1 value of PARAM MODACC to +1 for rigid
format 12, and to -1 for rigid format 11.

MTYPE - optional in blade cyclic modal flutter analysis (AERO rigid format
9). The BCD value of this parameter controls which components of the cyclic
modes are to be used in the modal formulation. MTYPE = SINE uses only sine
components and MTYPE = COSINE uses only cosine components. The default value
is COSINE.

NINPTS - optional in DISP static analysis (DISP rigid format 1). A positive
integer value of this parameter specifies the number of closest independent
points to be used in the interpolation for computing stresses or
strains/curvatures at grid points (only for TRIA1, TRIA2, QUAD1 and QUAD2
elements). A negative integer value or 0 specifies that all independent points
are to be used in the interpolation. The default value is 0.

NLOAD - optional in static analysis with cyclic symmetry (DISP rigid format
14). The integer value of this parameter is the number of static loading
conditions. The default value is 1.

NODJE - optional in all AERO rigid formats. A positive integer of this
parameter indicates that user-supplied downwash matrices due to extra points
are to be read from an external file via the INPUTT2 module in the rigid
format. The default value is -1 when not needed.

NSEGS - required in rigid formats using the cyclic symmetry feature (DISP
rigid formats 14 and 15 and AERO rigid format 9). The integer value of this
parameter is the number of identical segments in the structural model.

NT - optional in static analysis with differential stiffness and static
aerothermoelastic design/analysis of axial flow compressors (DISP rigid
formats 4 and 16). The integer value of this parameter limits the cumulative
number of iterations in both loops. The default value is 10 iterations.

OFFSET - a user warning message will be printed if the offset length of a BAR
element exceeds 15 percent of the bar length. This default value of 15 percent
can be changed by a PARAM OFFSET card.

OPT - optional in static and normal modes analyses (DISP rigid formats 1, 2,
3, 14, and 15). A positive integer value of this parameter causes both
equilibrium and multipoint constraint forces to be calculated for the Case
Control output request, MPCFORCE. A negative integer value of this parameter
causes only the equilibrium force balance to be calculated for the output
request. The default value is 0 which causes only the multipoint constraint
forces to be calculated for the output request.

P1, P2, and P3 - required in AERO rigid formats 10 and 11 when using NODJE
parameter. See Section 5.5 for a description of these parameters which are
required by the INPUTT2 module. The default values for P1, P2 and P3 are 0, 11
and XXXXXXXX, respectively.

PGEOM - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). The integer value of this parameter
specifies the punching of various bulk data cards. PGEOM = 1 causes the
punching of GRID bulk data cards. PGEOM = 2 causes the punching of GRID,
CTRIA2 and PTRIA2 bulk data cards. PGEOM = 3 causes the punching of GRID cards
and the modified ALGDB table on DTI cards. The default value of -1 suppresses
the punching of any of these cards.

POSITION, UNITNUM, and USRLABEL - required in AERO rigid format 9 when using
the NODJE parameter. See Section 5.5 for a description of these parameters
which are required by the INPUTT2 module. The default values for POSITION,
UNITNUM and USRLABEL are -1, 11 and TAPEID, respectively.

PRINT - optional in modal flutter analyses (AERO rigid formats 9 and 10). The
BCD value, NO, of this parameter suppresses the automatic printing of the
flutter summary for the K method. The default value is YESB in AERO rigid
format 9 and YES in AERO rigid format 10.

Q - required in aeroelastic response (AERO rigid format 11). The real value
of this parameter defines the dynamic pressure.

RADLIN - optional in transient heat transfer analysis (HEAT rigid format 9).
A positive integer value of this parameter causes some of the radiation
effects to be linearized (see Equation 2, Section 8.4.2 of the Theoretical
Manual). The default value is -1.

SIGMA - optional in nonlinear static (HEAT rigid format 3) and transient
(HEAT rigid format 9) heat transfer analyses. The real value of this parameter
is the Stefan-Boltzman constant. The default value is 0.0.

SIGN - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). The real value of this parameter controls
the type of run being performed. SIGN = 1.0 specifies a standard analysis type
run. SIGN = -1.0 specifies a design type run. The default value is 1.0.

STRAIN - optional in DISP static analysis (DISP rigid format 1). This
parameter controls the transformation of element strains/curvatures to the
material coordinate system (only for TRIA1, TRIA2, QUAD1 and QUAD2 elements).
If it is a positive integer, the strains/curvatures for these elements are
transformed to the material coordinate system. If it is zero,
strains/curvatures at the connected grid points are also computed in addition
to the element strains/curvatures in the material coordinate system. A
negative integer value results in no transformation of the strains/curvatures.
The default value is -1.

STREAML - optional in static aerothermoelastic design/analysis of axial flow
compressors (DISP rigid format 16). The integer value of this parameter
specifies the punching of various bulk data cards. STREAML = 1 causes the
punching of STREAML1 bulk data cards. STREAML = 2 causes the punching of
STREAML2 bulk data cards. STREAML = 3 causes both STREAML1 and STREAML2 cards
to be punched. The default value of -1 suppresses the punching of any of these
cards.

STRESS - optional in DISP static analysis (DISP rigid format 1). This
parameter controls the transformation of element stresses to the material
coordinate system (only for TRIA1, TRIA2, QUAD1 and QUAD2 elements). If it is
a positive integer, the stresses for these elements are transformed to the
material coordinate system. If it is zero, stresses at the connected grid
points are also computed in addition to the element stresses in the material
coordinate system. A negative integer value results in no transformation of
the stresses. The default value is -1.

SURFACE - optional in all DISP and AERO rigid formats. The computations of
the external surface areas for the two-dimensional and three-dimensional
elements are activated by this parameter when they are generated in the EMG
module. The results are multiplied by the real value of this parameter. See
the VOLUME parameter below for the case where the surface areas are to be
saved on an output file. The surface areas of the three-dimensional elements
are defined as follows.

  SURFACE AREA NO.                CORNER GRID POINTS USED

  Brick (8 or more grid points):
  1                               1, 2, 3, 4
  2                               1, 2, 6, 5
  3                               2, 3, 7, 6
  4                               3, 4, 8, 7
  5                               4, 1, 5, 8
  6                               5, 6, 7, 8

  Wedge (6 grid points):
  1                               1, 2, 3
  2                               1, 2, 5, 4
  3                               2, 3, 6, 5
  4                               3, 1, 4, 6
  5                               4, 5, 6

  Tetrahedron (4 grid points):
  1                               1, 2, 3
  2                               1, 2, 4
  3                               2, 3, 4
  4                               3, 1, 4

SYS21 - optional in DISP static analysis (DISP rigid format 1). This
parameter, like the INTERACT parameter, is of relevance only when your primary
purpose is to make interactive restart runs. In such a case, the integer value
of this parameter must be set to -1 (via a PARAM bulk data card) in the
interactive restart run (that follows a batch checkpoint run). If not so
specified via a PARAM bulk data card, the COMPOFF and COMPON instructions in
the DMAP sequence that use this parameter assume a value of 0 for this
parameter (see Section 5.7).

TABS - optional in nonlinear static (HEAT rigid format 3) and transient (HEAT
rigid format 9) heat transfer analyses. The real value of this parameter is
the absolute reference temperature. The default value is 0.0.

VOLUME - optional in all DISP and AERO rigid formats. The volume
computations for the two-dimensional and three-dimensional elements are
activated by this parameter when they are generated in the EMG module. The
results are multiplied by the real value of this parameter. If the 7th output
data block of the EMG module is specified (via DMAP ALTER), the element IDs,
volumes, surface areas (see the SURFACE parameter above), SIL, and grid point
coordinates are saved in the data block, a GINO-written file. If the 7th
output data block is one of the INPi (i=1,2,3,...,9,T) files, the same element
data is saved on a FORTRAN (binary)-written file. The following table
summarizes the data being saved.

  RECORD  WORDS                  CONTENTS

  0       1,2                    Header record, begins with GINO BCD name
          3-34                   Title, BCD
          35-66                  Sub-title, BCD
          67-98                  Label, BCD
          99-101                 Date, BCD

  1       1,2                    Element name of the first element, BCD
          3                      Element ID, integer
          4                      Volume (multiplied by scale factor n), or zero,
                                 real
          5                      (No. of surfaces)*100 + (No. of grid points),
                                 integer
          6                      Surface area of first surface, real
          :
          5+N                    Surface area of N-th surface, real
          5+N+1                  SIL of the first grid point, integer
          5+N+2,3,4              x,y,z coordinates of the first grid point, real
          :                      Repeat last 4 words for other grid points

  2                              A record similar to record 1 for the second 
                                 element
  :                              :
  LAST                           Last record (for the last element).

  The trailer of the output data block has the following information:
  Word 1 = LAST (No. of records written, header excluded),
  Words 2 through 6 contain no useful information.

VREF - optional in modal flutter analyses (AERO rigid formats 9 and 10).
Velocities are divided by the real value of this parameter to convert units or
to compute flutter indices. The default value is 1.0.

W3 and W4 - optional in the direct formulation of DISP transient response
problems (DISP rigid format 9). The real values (radians/unit time) of these
parameters are used as pivotal frequencies for uniform structural damping and
element structural damping, respectively (see Section 9.3.3 of the Theoretical
Manual). Parameter W3 is required if uniform structural damping is desired.
Parameter W4 is required if structural damping is desired for any of the
structural elements. Parameter W3 should not be used for hydroelastic
problems.

WTMASS - optional in all DISP and AERO rigid formats. The terms of the
structural mass matrix are multiplied by the real value of this parameter when
they are generated in the EMA module. Not recommended for use in hydroelastic
problems.

Example

Set double precision variable ABC to 1.23D+4 and add 5.6D-1 to it in the DMAP
module PARAMD. Result in DEF.

In executive control section,

  ALTER    n $
  PARAMD   //*ADD*/V,N,DEF/V,Y,ABC/5.6D-1  $
  ENDALTER   $

In bulk data section,

  PARAM, ABC  1.23D+4


# PBAR - Simple Beam Property

## Description

Defines the properties of a simple beam (bar) which is used to create bar
elements via the CBAR card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PBAR    |  PID  |  MID  |   A   |   I1  |   I2  |   J   |  NSM  |       |abc  |
|+bc     |   C1  |   C2  |   D1  |   D2  |   E1  |   E2  |   F1  |   F2  |def  |
|+ef     |   K1  |   K2  |  I12  |       |       |       |       |       |     |
|PBAR    |   39  |   6   |  2.9  |       |  5.97 |       |       |       |123  |
|+23     |       |       |  2.0  |  4.0  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

A         Area of bar cross-section (Real).

I1, I2, I12Area moments of inertia (Real, I1I2  >=  I212).

J         Torsional constant (Real).

NSM       Nonstructural mass per unit length (Real).

K1, K2    Area factor for shear (Real).

Ci, Di, Ei, Fi  Stress recovery coefficients (Real).

## Remarks

1.For structural problems, PBAR cards may only reference MAT1 material cards.

2.See Section 1.3.2 for a discussion of bar element geometry.

3.For heat transfer problems, PBAR cards may only reference MAT4 or MAT5
  material cards.

4.The quantities K1 and K2 are expressed as the relative amounts (0.0 to 1.0)
  of the total cross-sectional area contributing to the transverse shear
  stiffnesses (KAG) in the direction of the two principal axes. These
  quantities are ignored if I12 is non-zero. Defaults for K1 and K2 are: K1 =
  (12*E*I1)/(L*L*L); K2 = (12*EII2)/(L*L*L).


PCOMP - Layered Composite Element Property
==========================================

## Description

Defines the properties of an n-ply laminated composite material.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PCOMP   |  PID  |  ZOC  |  NSM  | SBOND |   FT  |       |       | LOPT  |abc  |
|+bc     | MID1  |  T1   |  TH1  | SOUT1 | MID2  |  T2   |  TH2  | SOUT2 |def  |
|+ef     | MID3  |  T3   |  TH3  | SOUT3 |           .. etc. .           |     |
|PCOMP   |  100  |  -0.5 |  1.5  |  5.+3 |  HOFF |       |       |SYMMEM |ABC  |
|+BC     | 150   |  0.05 |  90.  | YES   |       |       | -45.  |       |DEF  |
|+EF     |       |       |  45.0 |       |           .........           |     |

Field     Contents
-----     --------
PID       Property identification number (1,000,000 > Integer > 0).

ZOC       Offset of the element reference plane (element bottom surface)
          from the plane of grid points (Real or blank; see Guidelines below).

NSM       Non-structural mass per unit area (Real).

SBOND     Allowable shear stress of the bonding material. (Real > 0.0 or
          blank) Required if failure theory is used. (See Guidelines.)

FT        Failure theory, one of the strings HILL, HOFF, TSAI,
          STRESS, or STRAIN. See Remark 4. (BCD or blank).

LOPT      Lamination generation option, one of the strings ALL, SYM,
          MEM, or SYMMEM. See Remark 5. (BCD or blank). Default is
          ALL.

MIDi      Material identification number of the ith layer. (Integer >
          0 or blank).

Ti        Thickness of the ith layer (Real > 0.0 or blank).

THi       Angle between the longitudinal direction of the fibers of
          the ith layer and the material X-axis. (Real or blank).

SOUTi     Stress output request for ith layer, one of the strings YES
          or NO. (Default is NO).

## Remarks

1.      The plies are numbered from 1 to n beginning with the bottom layer.

2.      The offset (ZOC) is not the same offset (ZO) used in the CQUAD4 and
        CTRIA3 cards. ZOC references to the bottom surface of the element.

3.      SBOND is required if bonding material failure index calculations are
        desired.

4.      The failure theory is used to determine the element failure on a
        ply-by-ply basis. The available theories are:

        HILL             Hill Theory
        HOFF             Hoffman Theory
        TSAI             Tsai-Wu Theory
        STRESS           Maximum Stress Theory
        STRAIN           Maximum Strain Theory

5.      To minimize input requirements several lamination options (LOPT) are
        available. ALL indicates that every ply is specified. SYM indicates that
        ply layup is symmetric about the center ply and that the plies on one
        side of the center line are specified. SYMMEM indicates a symmetric
        layup of membrane only plies.

6.      The material properties, MIDi, may reference only MAT1, MAT2, and MAT8
        Bulk Data entries.

7.      If any of MIDi, Ti, or THi are blank, then the last non-blank values
        specified for each will be used to define the values for the ply.

Guidelines for the Use of PCOMP, PCOMP1, and PCOMP2

(Excerpt from "QUAD4 SEMINAR", WPAFB, WRDC-TR-89-3046, revised April 1993)

The purpose of PCOMP, PCOMP1, and PCOMP2 is to define element property
parameters in modeling laminated plates including layered (fiber reinforced)
composites. All three cards serve the same purpose except the options are
different. If the layers are made of different materials and the thicknesses
of the layers are all different, then the PCOMP card is appropriate. If all
the layers are made of the same material and thickness, then PCOMP1 is
appropriate. If the material is the same, but the thicknesses are different,
then PCOMP2 is appropriate. The first two fields on the PCOMP cards need no
further explanation.

Parameter ZOC

Parameter ZOC refers to the distance from the grid point surface to the bottom
of the plate. The plate bottom surface is defined in the diagrams at the end
of this section. It is the reference surface from which the stacking sequence
of the laminates is defined. Parameters ZO defined on QUAD4 and PSHELL are a
source of confusion sometimes. The offset parameters ZOC and ZO are not the
same entities.

Parameter SBOND

The bonding material shear stress is indirectly related to the interlaminar
shear and its value is generally empirical. A value of 400 to 500 psi for
SBOND appears to be reasonable in the absence of a value obtained from
experiments. Any approximation of this parameter will not affect the analysis
results. If affects only the Tsai-Wu failure theory, which is basically a 
post-processing function. 

Parameters MIDi, Ti, THi, and SOUTi

These parameters pertain to the ith layer. MID1 is the material identification
number of the first layer. The layer count goes up from the bottom surface of
the plate. The MIDi refers to one of three material cards: MAT1 for isotropic
materials, MAT2 for anisotropic materials, and MAT8 for orthotropic materials.
The parameter T1 defines the thickness of the first layer and TH1 refers to
the orientation of the material axis with reference to the material axis
defined on CQUAD4. SOUTi is the stress output parameter. Then the parameters
are repeated for all the layers unless the symmetry option is used under
parameter LOPT. If any MIDi, Ti, or THi are blank, then the last non-blank
values specified for each will be used.

Material Cards

Isotropic and Anisotropic MAT1 and MAT2

Orthotropic Material MAT8

Most of the parameters on MAT8 are self explanatory, with the exceptions of
G1Z and G2Z (fields 7 and 8). When these parameters are left blank, NASTRAN
assumes that the material is infinitely stiff in transverse shear and thus
overestimates the stiffness of the element. To avoid such overestimation,
transverse shear values have to be provided. Values of about two or three
orders of magnitude less than the modulus of elasticity of the material are
recommended.


PLAN VIEW:           G4             G3                 G3             G4
                      +--------------+                  +--------------+
                      |              |                  |              |
                      |              |                  |              |
                      |              |                  |              |
                      +--------------+                  +--------------+
                     G1             G2                 G2             G1

SIDE VIEW (PCOMP, PCOMP1,
           & PCOMP2):       TOP                               BOTTOM
                      +--------------+                  +--------------+- +
CASE 1                |              |                  |              |  |
                      |              |                  |              |  |
                      |              |                  |              |  |
                    + +--------------+                  +--------------+  | -ZOC
                +ZOC|     BOTTOM                              TOP         |
                    |                                                     |
                    + - - - - - - - - - GRID PT SURFACE  - - - - - - -  - +

                            TOP                              BOTTOM
                      +--------------+                  +--------------+- +
CASE 2                |              |                  |              |  | -ZOC
(DEFAULT)         + - - - - - - - - --  G.P. SURFACE - -- - - - - - - - - +
              -ZOC|   |              |                  |              |
                  + - +--------------+                  +--------------+
                           BOTTOM                              TOP

                  + - - - - - - - - -  GRID PT SURFACE  - - - - - - - - - +
                  |                                                       |
CASE 3            |                                                       | +ZOC
                  |         TOP                              BOTTOM       |
              -ZOC|   +--------------+                  +--------------+- +
                  |   |              |                  |              |
                  |   |              |                  |              |
                  |   |              |                  |              |
                  + - +--------------+                  +--------------+
                          BOTTOM                               TOP


PCOMP1 - Layered Composite Element Property
===========================================

## Description

Defines the properties of an n-ply laminated composite material where all
plies are composed of the same material and are of equal thickness.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PCOMP1  |  PID  |  ZOC  |  NSM  | SBOND |   FT  |  MID  | TPLY  | LOPT  |abc  |
|+bc     |  TH1  |  TH2  |  TH3  |  TH4  |  TH5  |       .. etc. .       |     |
|PCOMP1  |  100  |  -0.5 |  1.7  |  5.+3 |STRAIN |  200  | 0.25  | SYM   |ABC  |
|+EF     | -45.0 |  45.0 |  90.0 |  90.0 |  45.0 |                       |     |

Field     Contents
-----     --------
PID       Property identification number (1,000,000 > Integer > 0).

ZOC       Offset of the element reference plane (element bottom surface)
          from the plane of grid points (Real or blank; see Guidelines in
          PCOMP card).

NSM       Non-structural mass per unit area (Real).

SBOND     Allowable shear stress of the bonding material (Real > 0.0) (See
          Guidelines in PCOMP).

FT        Failure theory, one of the strings HILL, HOFF, TSAI, STRESS, or
          STRAIN. See Remark 4.

MID       Material identification number for all layers (Integer > 0).

LOPT      Lamination generation option, one of the strings ALL, SYM, MEM, or
          SYMMEM. See Remark 5.

TPLY      Thickness of all layers (Real > 0.0 or blank).

THi       Angle between the longitudinal direction of the fibers of the ith
          layer and the material X-axis (Real or blank).

## Remarks

1.The plies are numbered from 1 to n beginning with the bottom layer.

2.The offset (ZOC) is not the same offset (ZO) used in the CQUAD4 and CTRIA3
  cards. ZOC references the bottom surface of the element.

3.SBOND is required if bonding material failure index calculations are
  desired.

4.The failure theory is used to determine the element failure on a ply-by-ply
  basis. The available theories are:

  HILL    Hill Theory
  HOFF    Hoffman Theory
  TSAI    Tsai-Wu Theory
  STRESS  Maximum Stress Theory
  STRAIN  Maximum Strain Theory

5.To minimize input requirements several lamination options (LOPT) are
  available. ALL indicates that every ply is specified. SYM indicates that
  ply layup is symmetric about the center ply and that the plies on one side
  of the center line are specified. SYMMEM indicates a symmetric layup of
  membrane only plies.

6.The material property, MID, may reference only MAT1, MAT2, and MAT8 Bulk
  Data entries.

7.See "Guidelines for the Use of PCOMP, PCOMP1, and PCOMP2" in PCOMP card.


PCOMP2 - Layered Composite Element Property
===========================================

## Description

Defines the properties of an n-ply laminated composite material where all
plies are composed of the same material.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PCOMP2  |  PID  |  ZOC  |  NSM  | SBOND |   FT  |  MID  |       | LOPT  |abc  |
|+bc     |  T1   |  TH1  |  T2   |  TH2  |  T3   |  TH3  |    . etc. .   |     |
|PCOMP2  |  100  |  -0.5 |  1.7  |  5.+3 | TSAI  |  200  |       | SYM   |ABC  |
|+EF     |  0.25 | -45.0 |  0.5  |  90.0 |  0.25 |  45.0 |    ........   |     |

Field     Contents
-----     --------
PID       Property identification number (1,000,000 > Integer > 0).

ZOC       Offset of the element reference plane (element bottom surface)
          from the plane of grid points (Real or blank; see Guidelines in
          PCOMP card).

NSM       Non-structural mass per unit area (Real).

SBOND     Allowable shear stress of the bonding material (Real > 0.0) (See
          Guidelines in PCOMP).

FT        Failure theory, one of the strings HILL, HOFF, TSAI, STRESS, or
          STRAIN. See Remark 4.

MID       Material identification number for all layers (Integer > 0 or
          blank).

LOPT      Lamination generation option, one of the strings ALL, SYM, MEM, or
          SYMMEM. See Remark 5.

Ti        Thickness of the ith layer (Real > 0.0 or blank).

THi       Angle between the longitudinal direction of the fibers of the ith
          layer and the material X-axis (Real or blank).

## Remarks

1.The plies are numbered from 1 to n beginning with the bottom layer.

2.The offset (ZOC) is not the same offset (ZO) used in CQUAD4 and CTRIA3
  cards. ZOC references to the bottom surface of the element.

3.SBOND is required if bonding material failure index calculations are
  desired.

4.The failure theory is used to determine the element failure on a ply-by-ply
  basis. The available theories are:

  HILL    Hill Theory
  HOFF    Hoffman Theory
  TSAI    Tsai-Wu Theory
  STRESS  Maximum Stress Theory
  STRAIN  Maximum Strain Theory

5.To minimize input requirements several lamination options (LOPT) are
  available. ALL indicates that every ply is specified. SYM indicates that
  ply layup is symmetric about the center ply and that the plies on one side
  of the center line are specified. SYMMEM indicates a symmetric layup of
  membrane only plies.

6.The material property, MID, may reference only MAT1, MAT2, and MAT8 Bulk
  Data entries.

7.If any of the Ti or THi are blank, then the last non-blank values specified
  for each will be used to define the values for the ply.

8.See "Guidelines for the Use of PCOMP, PCOMP1, and PCOMP2" in PCOMP card.


PCONEAX - Conical Shell Element Property
========================================

## Description

Defines the properties of a conical shell element described on a CCONEAX card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PCONEAX |   ID  |  MID1 |   T1  |  MID2 |   I   |  MID3 |   T2  |  NSM  |+abc |
|+abc    |   Z1  |   Z2  |  PHI1 |  PHI2 |  PHI3 |  PHI4 |  PHI5 |  PHI6 |+def |
|+def    |  PHI7 |  PHI8 |  PHI9 | PHI10 | PHI11 | PHI12 | PHI13 | PHI14 |     |
|PCONEAX |   2   |   4   |  1.0  |   6   |  16.3 |   8   |  2.1  |  0.5  |+1   |
|+1      | 0.001 |-0.002 |  23.6 |  42.9 |       |       |       |       |+2   |
|+2      |       |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
ID        Property identification number (Unique Integer > 0).

MIDi      Material identification number for membrane, bending, and
          transverse shear (Integer >= 0).

T1, T2    Membrane thickness and transverse shear thickness (Real > 0.0 if
          MIDi not equal 0).

I         Moment of inertia per unit width (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress recovery (Real).

PHIi      Azimuthal coordinates (in degrees) for stress recovery (Real).

## Remarks

1.This card is allowed if and only if an AXIC card is also present.

2.PCONEAX cards may only reference MAT1 material cards.

3.If either MID1 = 0 or blank or T1 = 0.0 or blank, then both must be zero or
  blank.

4.If either MID2 = 0 or blank or I = 0.0 or blank, then both must be zero or
  blank.

5.If either MID3 = 0 or blank or T2 = 0.0 or blank, then both must be zero or
  blank.

6.A maximum of 14 azimuthal coordinates for stress recovery may be specified.
  An error will be detected if more than two continuation cards appear.

7.For a discussion of the conical shell problem, see Section 5.9 of the
  Theoretical Manual.


PDAMP - Scalar Damper Property
==============================

## Description

Used to define the damping value of a scalar damper element which is defined
by means of the CDAMP1 or CDAMP3 cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PDAMP   |  PID  |   B   |  PID  |   B   |  PID  |   B   |  PID  |   B   |     |
|PDAMP   |   14  | -2.3  |   2   |  6.1  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

B         Value of scalar damper (Real).

## Remarks

1.This card defines a damper value. Be careful when using negative damper
  values. Damper values are defined directly on the CDAMP2 and CDAMP4 cards.
  A structural viscous damper, CVISC, may also be used for geometric grid
  points.

2.Up to four damper properties may be defined on a single card.

3.For a discussion of scalar elements, see Section 5.6 of the Theoretical
  Manual.


PDUMi - Dummy Element Property
==============================

## Description

Defines the properties of a dummy element (1 <= i <= 9). Referenced by the
CDUMi card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PDUMi   |  PID  |  MID  |   A1  |   A2  |       |       | etc.  |       |abc  |
|+bc     |       | etc.  |   AN  |       |       |       |       |       |     |
|PDUM3   |  108  |   2   |  2.4  |  9.6  |  1.E4 |  15.  |       |  3.5  |ABC  |
|+BC     |   5   |       |    2  |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

A1...AN   Additional entries (Real or Integer).

## Remarks

1.The additional entries are defined in your element routines.


PELAS - Scalar Elastic Property
===============================

## Description

Used to define the stiffness, damping coefficient, and stress coefficient of a
scalar elastic element (spring) by means of the CELAS1 or CELAS3 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PELAS   |  PID  |   K   |   GE  |   S   |  PID  |   K   |   GE  |   S   |     |
|PELAS   |   7   | 4.29  | 0.06  | 7.92  |   27  |  2.17 |0.0032 |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

K         Elastic property value (Real).

GE        Damping coefficient, ge (Real).

S         Stress coefficient (Real).

## Remarks

1.Be careful using negative spring values. (Values are defined directly on
  some of the CELASi card types.)

2.One or two elastic spring properties may be defined on a single card.

3.For a discussion of scalar elements, see Section 5.6 of the Theoretical
  Manual.


PELBOW - Curved Beam or Elbow Property
======================================

## Description

Defines the properties of a curved beam or elbow element which is used to
create curved pipe or beam elements via the CELBOW card.

## Format and Example

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|PELBOW  |  PID  |  MID  |   A   |  I1   |  I2   |   J   |  NSM  |       |+abc |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|PELBOW  |   2   | 6061  | 16.0  | 211.0 | 211.0 | 422.0 |  6.0  |       |+P1  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+abc    |   r1  |   é1  |   r2  |   é2  |   r3  |   é3  |   r4  |   é4  |+def |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+P1     |   5.3 |  0.0  |   5.3 |  90.0 |   5.3 | 180.0 |   5.3 | 270.0 |+P2  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+def    |  K1   |  K2   |   C   |   Kx  |   Ky  |   Kz  |   R   |   +   |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+P2     |  2.0  |  2.0  |  1.0  |  1.0  |  5.76 |  5.76 |  15.0 |  90.0 |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

A         Area of cross section (Real > 0.0).

I1        Area moment of inertia in Plane 1 (Real).

I2        Area moment of inertia in Plane 2 (Real).

J         Torsional constant (Real).

NSM       Nonstructural mass per unit length (Real).

ri, éi    Stress recovery coefficients (Real, é in degrees) (See Figure 2.4-
          45.)

K1, K2    Area factors for shear (Real).

C         Stress intensification factor (Real).

Kx, Ky, KzFlexibility correction factors (Real).

R         Radius of curvature of the element (Real > 0.0).

+         Angle, in degrees, from GA to GB (Real, 0. < + < 180.) (See Figure
          2.4-45.)

## Remarks

1.For structural problems, PELBOW may only reference MAT1 cards.

2.For APP HEAT problems, PELBOW cards may only reference MAT4 or MAT5
  material cards.

3.The product moment of inertia is zero (I12 = 0). This assumes that at least
  one axis of symmetry of the element cross section exists, for example,
  tube, I-beam, channel, tee, etc.

4.See Section 1.3.2.2 for a discussion of the stress correction factor and
  the flexibility correction factors.

                Tb \
                      Fxb
                     \   . M2b
                      \.
                 GB  . . V2                        Plane 2
               V1b. .  M1b                                     Stress Recovery
                 .                                       Ze    Location
              .              Xe                          |   /
     |     .         ->      |                           |ri/
   C |  .  +   Ye    v       |   GA                      | /
     .-----------------------*                           |/ éi
     |\             M2a -----|----- V1a        ----------+--------- Xe  Plane 1
     | \                     |Fxa                        |
       Center of                                         |
       Curvature             |                           |
                             |Ta                         |


   Element Local Coordinate System              Element Cross-Section

                         Figure 2.4-45. PELBOW diagram


PERMBDY - Permeability Boundary
===============================

## Description

Specifies grid points on boundaries of dissimilar magnetic permeability.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PERMBDY |  G1   |  G2   |  G3   |  G4   |  G5   |  G6   |  G7   |  G8   |+a   |
|+a      |   G9  |  G10  |  G11  |  G12  |  G13  |  G14  |  G15  |  G16  |+b   |
|PERMBDY |   1   |   5   |   7   |   8   |  10   |  12   |  20   |  25   |+A   |
|+A      |   30  |  40   |  ENDT |       |       |       |       |       |     |

Field     Contents
-----     --------
Gi        Grid point identification numbers (Integers > 0).

## Remarks

1.There may be only one PERMBDY card.

2.The grid points on PERMBDY are those points which are on boundaries between
  elements of differing magnetic permeability.

3.The PERMBDY card is not required, but its use is recommended. See Section
  1.15.4.4 for more details.


# PFTUBE - Fluid Tube Property

## Description

Defines the parameter for the fluid tube element of the heat transfer model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PFTUBE  |  PID  |  CP   | VOLRT |  D1   |  D2   |       |       |       |     |
|PFTUBE  |   5   |  1.3  |  8.0  |  1.0  |  1.25 |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

CP        Heat capacity per unit volume (Real > 0).

VOLRT     Volume flow rate (Real >= 0).

D1        Diameter at inlet (Real > 0).

D2        Diameter at outlet (Real > 0 or blank). If blank, the value D1
          will be used.

## Remarks

1.The FTUBE element transports energy at the rate:

  Power = CP * VOLRT * U
                        inlet

  The heat capacity is given by:

  Energy = [+ * CP * (D1+D2)**2 * L /32] *(U +U )
                                            1  2

  where L is the distance between the connected grid points.


# PHBDY - Property of Heat Boundary Element

## Description

Defines the properties of the HBDY element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PHBDY   |  PID  |  MID  |   AF  |   E   | ALPHA |   R1  |   R2  |       |     |
|PHBDY   |  100  |  103  |  300. |  .79  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0 or blank), used for
          convective film coefficient and thermal capacity.

AF        Area factor (Real >= 0.0 or blank). Used only for HBDY types
          POINT, LINE, and ELCYL.

E         Emissivity (0.0 <= Real <= 1.0 or blank). Used only for radiation
          calculations.

ALPHA     Absorbtivity (0.0 <= Real <= l.0 or blank). Used only for thermal
          vector flux calculations; default value is E.

R1, R2    Radii of elliptic cylinder. Used for HBDY type ELCYL. See the HBDY
          element description. (Real).

## Remarks

1.The referenced material ID must be on a MAT4 card. The card defines the
  convective film coefficient and thermal capacity per unit area. If no
  material is referenced the element convection and heat capacity are zero.

2.The area factor AF is used to determine the effective area. For a POINT, AF
  = area; for LINE or ELCYL, AF = effective width where area = AF*length. For
  FTUBE, AF = +(R1 + R2)(length). The effective area is automatically
  calculated for other HBDY types.


PIHEX - Isoparametric Hexahedron Property
=========================================

## Description

Defines the properties of an isoparametric solid element, including a material
reference and the number of integration points. Referenced by the CIHEX1,
CIHEX2, and CIHEX3 cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PIHEX   |  PID  |  MID  |  CID  |  NIP  |   AR  |  ALFA |  BETA |       |     |
|PIHEX   |   15  |   3   |       |   3   |       |       |   5.0 |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

CID       Identification number of the coordinate system in which the
          material referenced by MID is defined (Integer >= 0 or blank).

NIP       Number of integration points along each edge of the element
          (Integer = 2, 3, 4, or blank).

AR        Maximum aspect ratio (ratio of longest to shortest edge) of the
          element (Real > 1.0 or blank).

ALFA      Maximum angle in degrees between the normals of two subtriangles
          comprising a quadrilateral face (Real, 0.0 <= ALFA <= 180.0, or
          blank).

BETA      Maximum angle in degrees between the vector connecting a corner
          point to an adjacent midside point and the vector connecting that
          midside point and the other midside or corner point (Real, 0.0 <=
          BETA <= 180.0, or blank).

## Remarks

1.All PIHEX cards must have unique identification numbers.

2.The default for NIP is 2 for IHEX1 and 3 for IHEX2 and IHEX3.

3.AR, ALFA, and BETA are used for checking the geometry of the element. The
  defaults are:

                   AR                ALFA                BETA
                                  (degrees)           (degrees)
  CIHEX1           5.0               45.0                 --

  CIHEX2          10.0               45.0                45.0

  CIHEX3          15.0               45.0                45.0

4.If CID = 0 or blank, MID must reference a MAT1 card. If CID > 0, MID must
  reference a MAT6 card (with or without reference to a MATT6 card).

5.If CID > 0, it must reference a rectangular coordinate system defined by a
  CORD1R or CORD2R card. (If a CORD2R card is used, the RID on that card must
  be 0 or blank.) Consequently, if MAT6 properties are to reference the basic
  coordinate system, a CORD1R or CORD2R card must be present to represent the
  basic coordinate system.

6.Non-zero CIDs on different PIHEX cards must be unique and must reference
  unique MAT6 MIDs.

7.If a MAT6 card is in the Bulk Data Deck, then it must be referenced on some
  PIHEX card and the following DMAP ALTER must be inserted following
  functional module GP1 in the rigid format DMAP sequence:

  ANISOP  GEOM1,EPT,BGPDT,EQEXIN,MPT/MPTA/S,N,ISOP $
  EQUIV    MPTA,MPT/ISOP  $

8.The restrictions represented by Remarks 4 through 7 above are expected to
  be removed in a future release of NASTRAN.


PIS2D8 - Quadratic Isoparametric Element Property
=================================================

## Description

Used to define the properties of a quadriparabolic isoparametric membrane
element. Referenced by the CIS2D8 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PIS2D8  |  PID  |  MID  |   T   |       |       |       |       |       |     |
|PIS2D8  |   2   |   1   |  0.5  |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness of membrane (Real).

## Remarks

1.All PIS2D8 cards must have unique property identification members.

2.The material property identification number must reference only a MAT1 or
  MAT2 card.

PLFACT - Piecewise Linear Analysis Factor Definition

## Description

Defines scale factors for piecewise linear analysis loading.

## Format and Example

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|PLFACT  |  SID  |  B1   |  B2   |  B3   |  B4   |  B5   |  B6   |  B7   |+abc |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|PLFACT  |   6   |  0.2  |  0.3  |  0.4  |  0.5  |  0.6  |  0.7  |  0.8  |ABC  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+abc    |   B8  |  B9   |  etc. |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+BC     |   0.9 |  1.0  |       |       |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field     Contents
-----     --------
SID       Unique set identification number (Integer > 0).

Bi        Loading factor (Real).

## Remarks

1.The remainder of the physical card containing the last entry must be null.

2.At any stage of the piecewise linear analysis, the accumulated load is
  given by

  {P } = B {P}
    i     i

  where {P} is the total load defined in the usual way. Example: If it were
  desired to load the structure in ten equally spaced load increments then
  one would set

  B   =  0.1 * i  ;  i = 1, 10
   i

3.Normally, the Bi form a monotonically increasing sequence. A singular
  stiffness matrix will result if Bi = Bi-1.

4.At least two factors must be defined.

5.Piecewise linear analysis factor sets must be selected in the Case Control
  Deck (PLCOEFF = SID) to be used by NASTRAN.


PLIMIT - Property Optimization Limits
=====================================

## Description

Defines the maximum and minimum limits for ratio of new property to original
property.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLIMIT  | ELTYP |  KMIN |  KMAX |  PID1 |  PID2 |  PID3 |  PID4 |  PID5 |+abc |
|+bc     |  PID6 |  etc. |       |       |       |       |       |       |     |
|PLIMIT  |  ROD  |   .01 |  1.5  |   1   |   3   |   5   |   4   |   2   |+ABC |
|+BC     |       |  etc. |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLIMIT  | ELTYP |  KMIN |  KMAX |  PID1 |"THRU" |  PIDi |       |       |     |
|PLIMIT  |  ALL  |  .001 | 0.05  |   30  | THRU  |  36   |       |       |     |

Field     Contents
-----     --------
ELTYP     One of the following element types: ROD, TUBE, BAR, TRMEM, QDMEM,
          TRPLT, QDPLT,TRBSC, TRIAl, QUAD1, TRIA2, QUAD2, SHEAR, or ALL or
          blank.

KMIN      Minimum property ratio (Real > 0.0 or blank).

KMAX      Maximum property ratio (Real > KMIN or = 0.0 or blank).

PIDn      List of property identification numbers associated with KMIN
          and/or KMAX (Integer > 0).

## Remarks

1.This card is not required (default KMIN = KMAX = 0.0 for ALL elements).

2.All PID values must be unique for each element type.

3.All elements with the same property identification number in the output
  stress data block, OES1, have these limits applied if ALL is specified.

4.Property entries optimized depend on the element type and material stress
  limits. Only nonzero properties with nonzero stress limits are optimized.

5.If KMAX = 0.0, no limit is placed on the maximum change.

6.If ELTYP is blank, ALL is assumed.

7.One of KMIN or KMAX may be blank but not both.


PLOAD - Static Pressure Load
============================

## Description

Defines a static pressure load.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD   |  SID  |   P   |   G1  |   G2  |   G3  |   G4  |       |       |     |
|PLOAD   |   1   | -4.0  |   16  |   32  |   11  |       |       |       |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

P         Pressure (Real).

G1,...,G4 Grid point identification numbers (Integer > 0; G4 may be zero).

## Remarks

1.Grid points must be unique and noncollinear.

2.If four grid points are given, four triangles are formed and half of P is
  applied to each one. For each triangle the direction is defined by

    ->    ->
  +(r   X r  )
     12    13

  where ij is the vector from Gi to Gj.

3.If three grid points are given, the pressure load is evenly distributed to
  the three grid points. The geometry of the triangle is not considered.

4.Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
  by NASTRAN.


PLOAD2 - Pressure Load
======================

## Description

Defines a uniform static pressure load applied to two-dimensional elements.
Only QUAD1, QUAD2, QUAD4, QDMEM, QDMEM1, QDMEM2, QDPLT, SHEAR, TRBSC, TRIA1,
TRIA2, TRIA3, TRMEM, TRPLT, or TWIST elements may have a pressure load applied
to them via this card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD2  |  SID  |   P   |  EID  |  EID  |  EIDm |"THRU" |  EIDn |  EID  |abc  |
|+bc     |  EID  |  etc. |       |       |       |       |       |       |def  |
|PLOAD2  |   21  | -3.6  |   1   |   4   |   16  |THRU   |   22  |   98  |ABC  |
|+BC     |  127  |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

P         Pressure value (Real).

EID,EIDm,EIDn  Element identification numbers (Integer > 0; EIDm < EIDn).

## Remarks

1.EID must be 0 or blank for omitted entries.

2.Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
  by NASTRAN.

3.At least one positive EID must be present on each PLOAD2 card.

4.The pressure load is computed for each element as if the grid points to
  which the element is connected were specified on a PLOAD card. The grid
  point sequence specified on the element connection card is assumed for the
  purpose of computing pressure loads.

5.All elements referenced must exist.

6.EID may be specified as individual references or as sequential lists
  (THRU sequences) and the two methods may be used interchangeably. The
  only restriction is that integer values must appear in fields 4 and 9 on
  the PLOAD2 card and in fields 2 and 9 on each continuation card (if all
  fields are used).

7.When the 88th word of SYSTEM is set to zero (the default), the PLOAD2
  pressure load is evenly distributed to the 3 corner grid points for all
  triangular elements. (That is, no element geometry is considered.) However,
  the distribution of the PLOAD2 pressure load on the quadrilateral elements
  is affected by the element geometry. If the 88th word of SYSTEM is set to
  1, then the load is distributed in proportion to the angle at each grid for
  the element.


PLOAD3 - Pressure Load on a Face of an Isoparametric Element
============================================================

## Description

Defines a uniform static pressure load applied to a surface of an
isoparametric hexahedron element only.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD3  |  SID  |   P   |  EID1 |  G11  |  G12  |  EID2 |  G21  |  G22  |     |
|PLOAD3  |   3   | -15.1 |   15  |   7   |   25  |   16  |  117  |  135  |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

P         Pressure value (Real, force per unit area).

EID1, EID2Element identification number (Integer > 0).

G11,G12; G21,G22  Grid point identification number of two grid points at
          diagonally opposite corners of the face on which the pressure acts
          (Integers > 0).

## Remarks

1.Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
  by NASTRAN.

2.At least one EID must be present on each PLOAD3 card.

3.All elements referenced must exist.

4.Computations consider the pressure to act positive outward on specified
  face of element.

PLOAD4 - Pressure Loads on Face of Structural Elements
======================================================

## Description

Defines a load on a face of a QUAD4 or CTRIA3 element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD4  |  SID  |  EID  |   P1  |  P2   |  P3   |   P4  |       |       |abc  |
|+bc     |  CID  |   N1  |   N2  |   N3  |       |       |       |       |     |
|PLOAD4  |  101  | 2043  |  15.  |  18.  |  23.6 |       |       |       |ABC  |
|+BC     |  52   |  1.0  |   0.  |   0.  |       |       |       |       |     |

Alternate Form 1:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD4  |  SID  |  E1   |   P1  |  P2   |  P3   |   P4  |"THRU" |E2     |ghi  |
|+hi     | CID   |  N1   |   N2  |   N3  |       |       |       |       |     |
|PLOAD4  | 1001  | 452   |  105. |       |       |       |THRU   | 568   |GHI  |
|+HI     | 2375  |  0.   |  1.   |  1.   |       |       |       |       |     |

Alternate Form 2:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD4  |  SID  |  P1   |  EID  |       |       |       |       |       |     |
|PLOAD4  |  101  |  15.  | 2042  |       |       |       |       |       |     |

Alternate Form 3:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOAD4  |  SID  |  P1   |  E1   |       |       |       |"THRU" |E2     |     |
|PLOAD4  | 1001  |  105. |  452  |       |       |       |THRU   |  568  |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

EID, E1, E2Element identification number (Integer > 0, E1 < E2).

Pi        Pressure at the grid point defining the element face (Real or
          blank).

CID       Coordinate system identification number (Integer >= 0).

Ni        Components of a vector in system CID that defines the direction
          (but not the magnitude) of the pressure (Real).

## Remarks

1.For the plate elements QUAD4 and TRIA3, if the continuation entry is not
  given, the direction of the pressure is normal to the element in the
  element Z direction, according to the right-hand-rule. If only P1 is given,
  the pressure is assumed to be uniform over the element surface.

2.If the loaded surface of an element is curved, and a direction vector is
  not specified, the direction of the pressure may vary over the surface. The
  pressure intensity is the load per unit surface area.

3.Equivalent grid point loads are computed. A uniform pressure need not
  result in equal grid point loads.

4.P4 is not used for plate element TRIA3.

5.Alternate Forms 2 and 3 are intended for quick conversion of PLOAD2 cards
  to PLOAD4. The continuation cards, not shown above, can also be used.

6.When PLOAD4 is applied to a surface, different resulting forces may exist
  if the surface is covered by QUAD4 elements or by TRIA3 elements. For
  example, if 12 psi is applied normal to a unit surface ABCD as shown, the
  resulting forces at four corners are tabulated as follows.


  D          C         D          C          D         C
  +----------+         +----------+          +---------+
  |          |         |       .  |          | .       |
  |          |         |     .    |          |   .     |
  |          |         |   .      |          |     .   |
  |          |         | .        |          |       . |
  +----------+         +----------+          +---------+
  A          B         A          B          A         B

    ONE QUAD4            TWO TRIA3             TWO TRIA3

                                        FORCES, LB. AT POINT
          ELEMENT(S)                     A     B     C     D

          ONE QUAD4 A-B-C-D             +3    +3     +3   +3
          ONE QUAD4 A-D-C-B             -3    -3     -3   -3

          TWO TRIA3 A-B-C, C-D-A        +4    +2     +4   +2
          TWO TRIA3 A-B-C, D-C-A         0    +2      0   +2
          TWO TRIA3 B-A-C, D-C-A         0    -2      0   -2
          TWO TRIA3 B-A-C, C-D-A        -4    -2     -4   -2

          TWO TRIA3 A-B-D, C-D-B        +2    +4     +2   +4
          TWO TRIA3 A-B-D, D-C-A        +2     0     +2    0
          TWO TRIA3 B-A-D, D-C-B        -2    -4     -2   -4
          TWO TRIA3 B-A-D, C-D-B        -2     0     -2    0


PLOTEL - Dummy Element Definition
=================================

## Description

Defines a dummy one-dimensional element for use in plotting. This element is
not used in the model during any of the solution phases of a problem. It is
used to simplify plotting of structures with large numbers of collinear grid
points where the plotting of each one along with the elements connecting them
would result in a confusing plot. The use of this "element" is entirely your
responsibility.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PLOTEL  |  EID  |   G1  |   G2  |       |  EID  |   G1  |   G2  |       |     |
|PLOTEL  |  29   |   35  |   16  |       |       |       |       |       |     |

Field     Contents
-----     --------
EID       Element identification number (Integer > 0).

G1, G2    Grid point identification numbers of connection points (Integer >
          0; G1 not equal G2).

## Remarks

1.Each element identification number must be unique with respect to all other
  element identification numbers.

2.One or two PLOTEL elements may be defined on a single card.

PMASS - Scalar Mass Property

## Description

Used to define the mass value of a scalar mass element which is defined by
means of the CMASS1 or CMASS3 cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PMASS   |  PID  |   M   |  PID  |   M   |  PID  |   M   |  PID  |   M   |     |
|PMASS   |   7   |  4.29 |   6   | 13.2  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

M         Value of scalar mass (Real).

## Remarks

1.This card defines a mass value. Be careful when using negative mass values.
  (Values are defined directly on some of the CMASSi card types.)

2.Up to four mass properties may be defined by this card.

3.For a discussion of scalar elements, see Section 5.6 of the Theoretical
  Manual.


POINTAX - Axisymmetric Point
============================

## Description

Defines the location of a point on an axisymmetric ring at which loads may be
applied via the FORCE, FORCEAX, MOMENT, or MOMAX cards and at which
displacements may be requested.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|POINTAX |   ID  |  RID  |  PHI  |       |       |       |       |       |     |
|POINTAX |   2   |   3   |  30.0 |       |       |       |       |       |     |

Field     Contents
-----     --------
ID        Point identification number (Unique Integer > 0).

RID       Identification number of a RINGAX card (Integer > 0).

PHI       Azimuthal angle in degrees (Real).

## Remarks

1.This card is allowed if and only if an AXIC card is also present.

2.Each POINTAX identification number must be unique with respect to all other
  POINTAX, RINGAX, and SECTAX identification numbers.

3.These points are not subject to constraints via MPCAX, SPCAX, or OMITAX
  card.

4.For a discussion of the conical shell problem, see Section 5.9 of the
  Theoretical Manual.

5.For a discussion of the axisymmetric solid problem, see Section 5.11 of the
  Theoretical Manual.

POPT - Property Optimization Parameter
======================================

## Description

Defines the basic parameters and existence of a property optimization
analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|POPT    |  MAX  |  EPS  |  GAMA | PRINT | PUNCH |       |       |       |     |
|POPT    |   2   |1.0E-3 |   0.9 |   2   |   NO  |       |       |       |     |

Field     Contents
-----     --------
MAX       Maximum number of iterations on property values (Integer > 0).

EPS       Convergence criteria for property value. If zero, no convergence
          check (Real >= 0.0).

GAMA      Iteration factor (Default = 1.0) (Real > 0.0).

PRINT     Print control for property parameters and OFP. Printout occurs
          every Ith loop.The first and last loops are always printed
          (Integer > 0).

PUNCH     Property card punch option. If YES, properties that were optimized
          are punched (BCD, YES or NO).

## Remarks

1.Only one POPT card is allowed.

2.All subcases will be analyzed MAX+1 times unless all properties converge.

3.Property convergence is defined by

   ||+|- + |
   || |   l|
  ----------- < EPS
     +
      l

  where + is the maximum stress and +l is the appropriate stress limit on the
  material card.

4.Stress recovery must be requested for one of the following elements: BAR,
  ELBOW, IS2D8, QDMEM, QDMEM1, QDMEM2, QDPLT, QUAD1, QUAD2, ROD, SHEAR,
  TRBSC, TRIA1, TRIA2, TRIM6, TRMEM, TRPLT, or TUBE. In addition, the
  material card must have stress limits defined.

5.Property cards are always printed for the last iteration.

6.The property entry optimized depends on the element type and the material
  stress limits (see Section 1.13).


PPSE - Pressure Stiffness Element Property
==========================================

## Description

Defines properties of a pressure stiffness element. Referenced by the CPSE2,
CPSE3, and CPSE4 cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PPSE    |  PID  |  P1   |   P2  |  P3   |  P4   |       |       |       |     |
|PPSE    |   1   |  500  |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

P1...P4   Applied pressure load, real.

## Remarks

1.All PPSE cards must have unique identification numbers.

2.P1 is the value of the applied pressure load. P2, P3, and P4 are reserved
  for possible future use with the CPSE2, CPSE3, and CPSE4 elements.

3.P1 is the  pressure force per unit length for the CPSE2 element, that is,
  lb/in. P1 is the pressure force per unit area for the CPSE3 and CPSE4
  elements, that is, psi.

4.See Remarks for the CPSEi elements.


PQDMEM - Quadrilateral Membrane Property
========================================

## Description

Used to define the properties of a quadrilateral membrane. Referenced by the
CQDMEM card. No bending properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQDMEM  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PQDMEM  |  235  |   2   |  0.5  |  0.0  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness of membrane (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PQDMEM cards must have unique property identification numbers.

2.One or two quadrilateral membrane properties may be defined on a single
  card.


PQDMEM1 - Isoparametric Quadrilateral Membrane Property
=======================================================

## Description

Used to define the properties of an isoparametric quadrilateral membrane.
Referenced by the CQDMEM1 card. No bending properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQDMEM1 |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PQDMEM1 |  235  |   2   |  0.5  |  0.0  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness of membrane (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PQDMEM1 cards must have unique property identification numbers.

2.One or two isoparametric quadrilateral membrane properties may be defined
  on a single card.


PQDMEM2 - Quadrilateral Membrane Property
=========================================

## Description

Used to define the properties of a quadrilateral membrane. Referenced by the
CQDMEM2 card. No bending properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQDMEM2 |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PQDMEM2 |  235  |   2   |  0.5  |  0.0  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness of membrane (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PQDMEM2 cards must have unique property identification numbers.

2.One or two quadrilateral membrane properties may be defined on a single
  card.

PQDPLT - Quadrilateral Plate Property
=====================================

## Description

Used to define the bending properties of a quadrilateral plate element.
Referenced by the CQDPLT card. No membrane properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQDPLT  |  PID  |  MID1 |   I   |  MID2 |   T   |  NSM  |   Z1  |   Z2  |     |
|PQDPLT  |   16  |   23  |  4.29 |   16  | 2.63  |1.982  |  0.05 | -0.05 |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for bending (Integer > 0).

I         Bending area moment of inertia per unit width (Real).

MID2      Material identification number for transverse shear (Integer >=
          0).

T         Transverse shear thickness (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress computation, positive according to the
          right-hand sequences defined on the CQDPLT card (Real).

## Remarks

1.All PQDPLT cards must have unique property identification numbers.

2.If T is zero, the element is assumed to be rigid in transverse shear.

3.No structural mass is generated for this element.

PQUAD1 - General Quadrilateral Element Property
===============================================

## Description

Defines the properties of a general quadrilateral element of the structural
model, including bending, membrane, and transverse shear effects. Referenced
by the CQUAD1 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQUAD1  |  PID  |  MID1 |   T1  |  MID2 |   I   |  MID3 |   T3  |  NSM  |abc  |
|+bc     |   Z1  |   Z2  |       |       |       |       |       |       |     |
|PQUAD1  |   32  |   16  |  2.98 |   9   | 6.45  |  16   |  5.29 |  6.32 |WXYZ1|
|+XYZ1   |  0.09 | -0.06 |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for membrane (Integer > 0).

T1        Membrane thickness (Real).

MID2      Material identification number for bending (Integer > 0).

I         Area moment of inertia per unit width (Real).

MID3      Material identification number for transverse shear (Integer >=
          0).

T3        Transverse shear thickness (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress computation, positive according to the
          right-hand sequence defined on the CQUAD1 card (Real).

## Remarks

1.All PQUAD1 cards must have unique property identification numbers.

2.If T3 is zero, the element is assumed to be rigid in transverse shear.

3.The membrane thickness, T1, is used to compute the structural mass for this
  element.


PQUAD2 - Homogeneous Quadrilateral Property
===========================================

## Description

Defines the properties of a homogeneous quadrilateral element of the
structural model, including bending, membrane and transverse shear effects.
Referenced by the CQUAD2 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PQUAD2  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PQUAD2  |   32  |   16  |  2.98 |  9.0  |   45  |   16  |  5.29 |  6.32 |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PQUAD2 cards must have unique identification numbers.

2.The thickness used to compute membrane and transverse shear properties is
  T.

3.The area moment of inertia per unit width used to compute the bending
  stiffness is (T**3)/12.

4.Outer fiber distances of plus or minus T/2 are assumed.

5.One or two homogeneous quadrilateral properties may be defined on a single
  card.


PRESAX - Axisymmetric Pressure Load
===================================

## Description

Defines the static pressure loading for a model containing CCONEAX, CTRAPAX,
or CTRIAAX elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PRESAX  |  SID  |   P   |  RID1 |  RID2 |  PHI1 |  PHI2 |       |       |     |
|PRESAX  |   3   |  7.92 |   4   |   3   |  20.6 |  31.4 |       |       |     |

Field     Contents
-----     --------
SID       Load set identification number (Integer > 0).

P         Pressure value (Real).

RID1, RID2Ring identification numbers (see RINGAX card) (Integer > 0).

PHI1, PHI2Azimuthal angles in degrees (Real, PHI1 not equal PHI2).

## Remarks

1.This card is allowed if and only if an AXIC card is also present.

2.Load sets must be selected in the Case Control Deck (LOAD = SID) in order
  to be used by NASTRAN.

3.For a discussion of the conical shell problem, see Section 5.9 of the
  Theoretical Manual.

4.For a discussion of the axisymmetric solid problem, see Section 5.11 of the
  Theoretical Manual.

PRESPT - Fluid Pressure Point
=============================

## Description

Defines the location of pressure points in the fluid for recovery of pressure
data.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PRESPT  |  IDF  |       |  IDP  |   í   |  IDP  |   í   |  IDP  |   í   |     |
|PRESPT  |   14  |       |  141  |  0.0  |       |       |  142  |  90.0 |     |

Field     Contents
-----     --------
IDF       Fluid point (RINGFL) identification number (Integer > 0).

IDP       Unique pressure point identification number (Integer > 0).

í         Azimuthal position on fluid point, referenced by IDF, in fluid
          coordinate system (Real).

## Remarks

1.This card is allowed only if an AXIF card is also present.

2.All pressure point identification numbers must be unique with respect to
  other scalar, structural, and fluid points.

3.The pressure points are used primarily for the identification of output
  data. They may also be used as points at which to measure pressure for
  input to control devices (see User's Manual, Section 1.7).

4.One, two, or three pressure points may be defined per card.

5.Output requests for velocity and acceleration of these degrees of freedom
  will result in derivatives of pressure with respect to time.

PROD - Rod Property
===================

## Description

Defines the properties of a rod which is referenced by the CROD card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PROD    |  PID  |  MID  |   A   |   J   |   C   |  NSM  |       |       |     |
|PROD    |   17  |   23  |  42.6 | 17.92 | 4.236 |  0.5  |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

A         Area of rod (Real).

J         Torsional constant (Real).

C         Coefficient to determine torsional stress (Real).

NSM       Nonstructural mass per unit length (Real).

## Remarks

1.PROD cards must all have unique property identification numbers.

2.For structural problems, PROD cards may only reference MAT1 material cards.

3.For heat transfer problems, PROD cards may only reference MAT4 or MAT5
  cards.

PROLATE - Prolate Spheroidal Surface
====================================

## Description

Specifies a prolate spheroidal surface of the finite element model in
magnetostatics problems.

Format

+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
+========+=======+=======+=======+=======+=======+=======+=======+=======+=====+
|PROLATE |   A   |   B   | NSEGS | MSEGS |  NN   |  NM   |  G1   |  G2   |+P1  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+P1     |  G3   |  G4   |   .   |   .   |   .   |   .   |   .   |   .   |+P2  |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
                     .                                      .
                     .                                      .
                     .                                      .
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+
|+PN     |   .   |   .   |   .   | ENDT  |       |       |       |       |     |
+--------+-------+-------+-------+-------+-------+-------+-------+-------+-----+

Field     Contents
-----     --------
A         Length of semi-major axis of generating ellipse (Real > 0.0).

B         Length of semi-minor axis of generating ellipse (Real > 0.0, B <
          A).

NSEGS     Number of segments in longitudinal direction (Integer > 2).

MSEGS     Number of segments in circumferential direction (Integer > 2).

NN, NM    Maximum n,m in series expansion (Integer, 1 < NM < NN < 30) (see
          Equation 10 in Section 1.15.3).

G1        Grid point identification number at left end point (Integer > 0).

G2        Grid point Identification number at right end point (Integer > 0).

Gi, i >= 3Grid point identification numbers of points defining the prolate
          spheroidal surface (Integer > 0).

## Remarks

1.The major axis of the generating ellipse must lie on the X-axis of the
  basic coordinate system, the minor axis must lie on the Y-axis of the basic
  coordinate system, and the center of the ellipse must coincide with the
  origin of the basic coordinate system.

                  YBASIC
                  |
                  |
                  |
                  |Z
  ----------------+----------------- XBASIC
                  |
                  |
                  |
                  |

                                                      YBASIC
                                                      |
                                                  \ é |
                                                   \  |
                                                    \ |
                                                     \|
                                ZBASIC  --------------+ XBASIC

2.The ordering of the grid points on the PROLATE card is crucial and must
  conform to the order given in Figure 2.4-46 below (although the actual
  numbers will vary depending on the number of longitudinal segments). Note
  that the first set of grid points specified (starting with G3) corresponds
  to the start of the first circumferential segment, which must be in the X-Y
  plane at é = 0 degree in the prolate spheroidal coordinate system.

3.The number of longitudinal segments must be the same for every
  circumferential segment.

4.The PROLATE computations are set up to handle either 180 degree or 360
  degree modeling of the prolate spheroidal surface. The 180 degree modeling
  is assumed if Case Control card AXISYM contains SYMM or ANTI (with or
  without the ANOM option), indicating symmetry of the finite element model
  about the X-Y plane, and symmetry or antisymmetry, respectively of the
  source magnetic field and, therefore, of the anomaly potential, about the
  X-Y plane.

5.The total number of grid points on the PROLATE card must be
  (NSEGS-1)(MSEGS+1) + 2 if 180 degree modeling is used and (NSEGS-1)(MSEGS)
  + 2 if 360 degree modeling is used.

6.In 360 degree modeling, the grid points at 0 degrees (G3 through GN in the
  sketch) are also the grid points at 360 degrees. However, on the PROLATE
  card, this set of points must not be repeated. With 360 degree modeling,
  the final set of grid points on the PROLATE card must consist of those at
  the end of the (MSEGS-1)th segment.

7.Only one PROLATE card is allowed.

                                     Y
                                     +---------------------------+-
                            G6.      |     .                     |
                       G5.           |          .                |
                   G4.               |              .            B
                G3.                  |                  .GN      |
             G1.                     |                      .G2 -+---- X
                                     |
                                     |                      |
                                     +---------- A ---------+
                                     |                      |

                        Figure 2.4-46. PROLATE diagram


# PSHEAR - Shear Panel Property

## Description

Defines the elastic properties of a shear panel. Referenced by the CSHEAR
card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PSHEAR  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PSHEAR  |   13  |   2   |  4.9  |  16.2 |  14   |   6   |  4.9  |  14.7 |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness of shear panel (Real not equal 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PSHEAR cards must have unique identification numbers.

2.PSHEAR cards may only reference MAT1 material cards.

3.One or two shear panel properties may be defined on a single card.


PSHELL - Shell Element Property
===============================

## Description

Defines the membrane, bending, transverse shear, and coupling properties of
the QUAD4 and CTRIA3 shell elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PSHELL  |  PID  |  MID1 |   T   |  MID2 |12I/T3 |  MID3 | TS/T  |  NSM  |abc  |
|+bc     |   Z1  |   Z2  |  MID4 | MCSID | SCSID |   ZO  |       |       |     |
|PSHELL  |  203  |  204  |  1.90 |  205  |  1.2  |  206  |  0.8  |  6.32 |ABC  |
|+BC     |  +.95 |  -.95 |       |   0   |   0   |  0.01 |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for membrane (Integer > 0 or
          blank).

T         Default value for membrane thickness (Real > 0.0; see
          Guidelines).

MID2      Material identification number for bending (Integer > 0 or
          blank).

12I/T3    Bending stiffness parameter (Real or blank, default =
          1.0; see Guidelines).

MID3      Material identification number for transverse shear (Integer
          > 0 or blank; must be blank unless MID2 > 0).

TS/T      Transverse shear thickness divided by membrane thickness
          (Real or blank; default = 0.833333; see Guidelines).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress computation. The positive
          direction is determined by the righthand rule and the
          order in which the grid points are listed on the
          connection entry. (Real or blank; defaults are -T/2
          for Z1 and +T/2 for Z2.)

MID4      Material identification number for membrane-bending coupling
          (Integer > 0 or blank; must be blank unless MID1 > 0 and
          MID2 > 0; may not equal MID1 or MID2.)

MCSID     Identification number of material coordinate system (Real or
          blank, or Integer > 0) (See Remark 10).

SCSID     Identification number of stress coordinate system (Real or
          blank, or Integer > 0) (See Remark 10).

ZO        Offset of the element reference plane (element
          mid-plane) from the plane of grid points. (Real or
          blank; default = .0. See Remark 11 and Guidelines in
          CQUAD4).

## Remarks

1.      All PSHELL property entries must have unique identification numbers.

2.      The structural mass is computed from the density using the membrane
        thickness and membrane material properties.

3.      The results of leaving any MID field blank are:

        MID1     No membrane or coupling stiffness; no structural mass; no
                 structural damping

        MID2     No bending, coupling, or transverse shear stiffness

        MID3     No transverse shear flexibility

        MID4     No membrane-bending coupling

4.      The continuation entry is not required.

5.      Structural damping, when needed, is obtained from the MID1 material.

6.      The MID4 field should be left blank if the material properties are
        symmetric with the middle surface of the shell.

7.      For structural problems, PSHELL entries may reference MAT1, MAT2, or
        MAT8 material property data.

8.      If the transverse shear material, MID3, references MAT2 data, then G33
        must be zero. If MID3 references MAT8 data, then G1,Z and G2,Z must not
        be zero.

9.      For heat transfer problems PSHELL entries may reference MAT4 or MAT5
        material property data.

10.     If MCSID/SCSID is left blank (0.0) or is Real, it is considered to be
        the angle of rotation of the X axis of the material/stress coordinate
        system with respect to the X axis of the element coordinate system in
        the XY plane of the latter. If Integer, the orientation of the
        material/stress x-axis is along the projection of the x-axis of the
        specified coordinate system onto the x-y plane of the element system.
        The value of MCSID is the default value for the TM field on the CQUAD4
        or CTRIA3 Bulk Data entries.

11.     The value of ZO is the default value for the corresponding field on the
        CQUAD4 or CTRIA3 Bulk Data entries.

## Guidelines for the Use of PSHELL

(Excerpt from "QUAD4 SEMINAR", WPAFB, WRDC-TR-89-3046, revised April 1993)

PSHELL or PCOMP are the property cards referenced on the CQUAD4 (in field 3).
PSHELL is to be used when the plate is not laminated (or layered), while PCOMP
is for laminated plates. Only one of these is applicable for a given element.
The diagram below illustrates key features of the elements described on the
PSHELL card. It is a sandwich plate with two face sheets separated by a
honeycomb core.

            +---+---------------------------------+
        T/2 |   |/////////////////////////////////| \
            +---+---------------------------------+  \
            |   |                                 |   \
         TS |   |        HONEYCOMB CORE           |     FACE SHEETS
            |   |                                 |   /
            +---+---------------------------------+  /
        T/2 |   |/////////////////////////////////| /
            +---+---------------------------------+

The first two fields of the PSHELL card are for the name and property
identification called from CQUAD4. The third field, MID1, is the material
identification number for the face sheets in membrane behavior. Parameter T is
the total thickness of the two face sheets. MID2 is the material
identification number for bending behavior, MID3 for shear, and MID4 for
membrane-bending coupling. There are two types of membrane-bending coupling.
The coupling resulting from asymmetry in plate construction (non-symmetric
laminates) is called linear coupling. Nonlinear coupling, on the other hand,
is a result of the interaction of internal forces such as inplane and out of
plane (beam-column effect) forces. The latter coupling can be accounted for
only in differential stiffness and/or buckling analysis. Parameter 12I/T3
(field 6) can be calculated by using the following definition for I:

              +þ                    þ+
              |  1  T 3   T TS   T 2 |
        I = 2 | ---(-)  + -(-- + -)  |
              | 12  2     2  2   4   |
              +þ                    þ+

I is basically the moment of inertia of the face sheets about the neutral axis
(centroidal). It is assumed that the face sheets are symmetric about the
neutral axis. If they are not, the moment of inertia about the neutral axis
can be calculated. For solid plates this parameter is simply 1.0.

The definition of parameter TS/T is obvious from the diagram.

Parameters MCSID and SCSID refer to the material coordinate system. There are
two options for this definition. By leaving the field blank or a real value
the first option is invoked. In this option the parameter represents the angle
between the side of the element connecting the grid points G1 and G2 and the
material axis. The second option is an integer which refers to a coordinate
system defined on a COORD card. The second option is the most desirable
because the grid point sequence on the CQUAD4 card does not affect the
material axis.

Offset parameter ZO is the same as defined on CQUAD4. The entry on CQUAD4,
however, overrides that on the PSHELL card.

The PSHELL card provides the facility to model homogeneous as well as sandwich
plates. However, the face sheets of the sandwich plates are assumed to be
homogeneous (isotropic, orthotropic, or anisotropic) plates. Modeling sandwich
plates with face sheets made of layered composites requires some additional
effort. A two-step method involving a DMAP alter is recommended.

Sandwich Plates with Composite Face Sheets

Step 1: Modeling the face sheets with PCOMP cards, make a NASTRAN run with
Rigid Format ALTER, and exit NASTRAN after EMG module, that is

        ALTER 39 $  reference 39 could change from solution to solution
        JUMP FINIS $
        ENDALTER  $

This run will put out an equivalent PSHELL card along with four MAT2 cards.

Step 2: This step involves modification of the PSHELL cards generated in Step
1 and the introduction of a new MAT8 card. The PCOMP cards used in Step 1 must
be eliminated from this Step 2 run.

The property parameters G11, G22, and G12 from the first MAT2 card (output
from Step 1 run) become E1, E2, and G12 on the new MAT8 card. The remaining
parameters are defined as above. This new MAT8 card applies to both membrane
and bending (MID1 and MID2) on the PSHELL card (also output from Step 1). The
material identification MID3 on the PSHELL card refers to the shear behavior
of the honeycomb core. This refers to a MAT1 card and with only shear modulus
G defined. The Young's modulus E, the Poisson's ratio v, should be left blank.
This requirement is mandatory when modeling a honeycomb core with MAT1 card;
otherwise the results would be wrong. The shear modulus G for the honeycomb
core should be obtained from honeycomb manufacturer's handbooks such as
HEXCEL. In the absence of such information, an approximate G value of two to
three orders of magnitude less than the modulus of elasticity E can be used.
The other three MaT2 cards from Step 1 run are not used in Step 2, and can be
discarded.

Membrane, bending, and shear deformations are included even though only MID2
is specified. Membrane and bending behavior are computed by the material
properties called from MID2. It is important to note that the shear
deformation is computed by assuming a material infinitely stiff in transverse
shear when the MID3 field is blank. The easiest way to avoid shear stiffness
over-estimation is not to leave MID3 blank when MID2 is specified.


PTORDRG - Toroidal Ring Property
================================

## Description

Used to define membrane and flexure (bending) properties of a toroidal ring
element. Referenced by the CTORDRG card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTORDRG |  PID  |  MID  |   TM  |   TF  |  PID  |  MID  |   TM  |   TF  |     |
|PTORDRG |   2   |   4   |  0.1  |  0.15 |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

TM        Thickness for membrane (Real > 0.0).

TF        Thickness for flexure (Real).

## Remarks

1.All PTORDRG cards must have unique property identification numbers.

2.The material identification number MID must reference only a MAT1 or MAT3
  card.

3.One or two toroidal ring properties may be defined on a single card.


PTRAPAX - Triangular Ring Element Property
==========================================

## Description

Defines the properties of an axisymmetric trapezoidal cross-section ring
element referenced by the CTRAPAX card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRAPAX |  PID  |       |  MID  | PHI1  | PHI2  | PHI3  | PHI4  | PHI5  |+abc |
|+abc    | PHI6  | PHI7  | PHI8  | PHI9  | PHI10 | PHI11 | PHI12 | PHI13 |+def |
|+def    | PHI14 |       |       |       |       |       |       |       |     |
|PTRAPAX |   5   |       |  15   |  0.0  |  5.0  |  6.0  |  7.0  |  8.0  |+N1  |
|+N1     | 9.0   | 10.0  | 15.0  | 20.0  | 25.0  | 30.0  | 35.0  | 40.0  |+N2  |
|+N2     | 45.0  |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

PHIi      Azimuthal coordinates (in degrees) for stress recovery (Real).

## Remarks

1.All PTRAPAX cards must have unique property identification numbers.

2.This card is allowed if and only if an AXIC card is also present.

3.PTRAPAX card may reference MAT1 or MAT3 material cards.

4.A maximum of 14 azimuthal coordinates for stress recovery may be specified.


PTRBSC - Basic Bending Triangle Property
========================================

## Description

Defines basic bending triangle (TRBSC) properties. Referenced by the CTRBSC
card. No membrane properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRBSC  |  PID  |  MID1 |   I   |  MID2 |   T   |  NSM  |   Z1  |   Z2  |     |
|PTRBSC  |   3   |   17  |  6.29 |   4   |  16.  | 1.982 |  0.05 | -0.05 |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for bending (Integer > 0).

I         Bending area moment of inertia per unit width (Real).

MID2      Material identification number for transverse shear (Integer >=
          0).

T         Transverse shear thickness (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for shear computation, positive according to the
          right-hand sequence defined in the CTRBSC card (Real).

## Remarks

1.All PTRBSC cards must have unique property identification numbers.

2.If T is zero, the element is assumed to be rigid in transverse shear.

3.No structural mass is generated by this element.


PTRIA1 - General Triangular Element Property
============================================

## Description

Defines the properties of a general triangular element of the structural
model, including bending, membrane and transverse shear effects. Referenced by
the CTRIA1 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRIA1  |  PID  |  MID1 |   T1  |  MID2 |   I   |  MID3 |   T3  |  NSM  |abc  |
|+bc     |   Z1  |   Z2  |       |       |       |       |       |       |     |
|PTRIA1  |   32  |   16  |  2.98 |   9   |  6.45 |   16  |  5.29 |  6.32 |QED  |
|+ED     |       |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for membrane (Integer >= 0).

T1        Membrane thickness (Real).

MID2      Material identification number for bending (Integer >= 0).

I         Area of moment of inertia per unit width (Real).

MID3      Bending material identification number for transverse shear
          (Integer >= 0).

T3        Transverse shear thickness (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress calculations, positive according to the
          right-hand sequence defined on the CTRIA1 card (Real).

## Remarks

1.All PTRIA1 cards must have unique property identification numbers.

2.If T3 is zero, the element is assumed to be rigid in transverse shear.

3.The membrane thickness, T1, is used to compute the structural mass for this
  element.

PTRIA2 - Homogeneous Triangular Element Property
================================================

## Description

Defines the properties of a homogeneous triangular element of the structural
model, including membrane, bending and transverse shear effects. Referenced by
the CTRIA2 card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRIA2  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PTRIA2  |   2   |   16  | 3.92  |  14.7 |   6   |   16  |  2.96 |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Thickness (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PTRIA2 cards must have unique identification numbers.

2.The thickness used to compute the membrane and transverse shear properties
  is T.

3.The area moment of inertia per unit width used to compute the bending
  stiffness is (T**3)/12.

4.Outer fiber distances of plus or minus T/2 are assumed.

5.One or two homogeneous triangular element properties may be defined on a
  single card.

PTRIAAX - Triangular Ring Element Property
==========================================

## Description

Defines the properties of an axisymmetric triangular cross-section ring
element referenced by the CTRIAAX card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRIAAX |  PID  |       |  MID  | PHI1  | PHI2  | PHI3  | PHI4  | PHI5  |+abc |
|+abc    | PHI6  | PHI7  | PHI8  | PHI9  | PHI10 | PHI11 | PHI12 | PHI13 |+def |
|+def    | PHI14 |       |       |       |       |       |       |       |     |
|PTRIAAX |   5   |       |  15   |  0.0  |  5.0  |  6.0  |  7.0  |  8.0  |+N1  |
|+N1     | 9.0   | 10.0  | 15.0  | 20.0  | 25.0  | 30.0  | 35.0  | 40.0  |+N2  |
|+N2     | 45.0  |       |       |       |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

PHIi      Azimuthal coordinates (in degrees) for stress recovery (Real).

## Remarks

1.All PTRIAAX cards must have unique property identification numbers.

2.This card is allowed if and only if an AXIC card is also present.

3.PTRIAAX card may reference MAT1 or MAT3 material cards.

4.A maximum of 14 azimuthal coordinates for stress recovery may be specified.


PTRIM6 - Linear Strain Triangular Membrane Property
===================================================

## Description

Defines the properties of a linear strain triangular membrane element.
Referenced by the CTRIM6 card. No bending properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRIM6  |  PID  |  MID  |   T1  |   T3  |   T5  |  NSM  |       |       |     |
|PTRIM6  |  666  |  999  |  1.17 |  2.52 |  3.84 |  8.3  |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T1, T3, T5Membrane thicknesses at the vertices of the element (Real).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PTRIM6 cards must have unique property identification numbers

2.PTRIM6 cards may only reference MAT1 or MAT2 cards.

3.In general, the thickness varies linearly over the triangle. If T3 or T5 is
  specified 0.0 or blank, it will be set equal to T1.


PTRMEM - Triangular Membrane Property
=====================================

## Description

Used to define the properties of a triangular membrane element. Referenced by
the CTRMEM card. No bending properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRMEM  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PTRMEM  |   17  |   23  | 4.25  |  0.2  |       |       |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID       Material identification number (Integer > 0).

T         Membrane thickness (Real > 0.0).

NSM       Nonstructural mass per unit area (Real).

## Remarks

1.All PTRMEM cards must have unique property identification numbers.

2.One or two triangular membrane properties may be defined on a single card.


PTRPLT - Triangular Plate Property
==================================

## Description

Used to define the bending properties of a triangular plate element.
Referenced by the CTRPLT card. No membrane properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRPLT  |  PID  |  MID1 |   I   |  MID2 |   T   |  NSM  |   Z1  |   Z2  |     |
|PTRPLT  |   17  |   26  |  4.29 |   16  | 3.9-4 | 2.634 |       |       |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for bending (Integer > 0).

I         Bending area moment of inertia per unit width (Real).

MID2      Material identification number for transverse shear (Integer >=
          0).

T         Transverse shear thickness (Real).

NSM       Nonstructural mass per unit area (Real).

Z1, Z2    Fiber distances for stress computation, positive according to the
          right-hand sequence defined on the CTRPLT card (Real).

## Remarks

1.All PTRPLT cards must have unique property identification numbers.

2.If T is zero, the element is assumed to be rigid in transverse shear.

3.No structural mass is generated by this element.


PTRPLT1 - Triangular Plate Property
===================================

## Description

Defines the bending properties of a higher order triangular plate element.
Referenced by the CTRPLT1 card. No membrane properties are included.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRPLT1 |  PID  |  MID1 |   R1  |   R3  |   R5  |  MID2 |  TS1  |  TS3  |abc  |
|+bc     |  TS5  |  NSM  |  Z11  |  Z21  |  Z13  |  Z23  |  Z15  |  Z25  |     |
|PTRPLT1 |   15  |   25  |  20.0 |  30.0 |  40.0 |   35  |  3.0  |  1.15 |PQR  |
|+QR     |  1.0  |  9.0  |  1.5  | -1.5  |  2.0  | -2.0  | +2.5  | -2.5  |     |

Field     Contents
-----     --------
PID       Property identification number (Integer > 0).

MID1      Material identification number for bending (Integer > 0).

R1, R3, R5Area moment of inertia per unit width at the grid points G1, G3,
          and G5 respectively (Real > 0.0); R1 = T1**3/12, R3 = T3**3/12, R5
          = T5**3/12 where T1, T3, and T5 are the membrane thicknesses of
          the element at the vertices, respectively.

MID2        Material identification number for transverse shear (Integer > 0).

TS1, TS3, TS5  Transverse shear thicknesses at the grid points G1, G3, and G5,
            respectively (Real).

NSM         Nonstructural mass per unit area (Real).

Z11, Z21, Z13; Z23, Z15, Z25  Fiber distances for stress computation at grid 
            points G1, G3, and G5, respectively; positive according to the
            right-hand sequence defined on the CTRPLT1 card (Real).

## Remarks

1. All PTRPLT1 cards must have unique property identification numbers.

2. If TS1 is zero, the element is assumed to be rigid in transverse shear.

3. If TS3 or TS5 is 0.0 or blank, it will be set equal to TS1.

4. If T3 or T5 is 0.0 (that is, R3 or R5 are 0.0 or blank), it will be set
   equal to T1. (T1, T3, and T5 are computed from R1, R3, and R5)

5. The stresses at the centroid will be computed at the top and bottom fibers.
   The stresses at G1, G3, and G5 will be computed at the locations defined on
   the property card (if given).

6. The continuation card is required, even if blank.


PTRSHL - Higher Order Triangular Shell Element Property
=======================================================

## Description

Defines the membrane bending and transverse shear properties of a higher order
triangular shell element. Referenced by the CTRSHL card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTRSHL  |  PID  |  MID1 |   T1  |   T3  |   T5  |  MID2 |   I1  |   I3  |abc  |
|+bc     |   I5  |  MID3 |  TS1  |  TS3  |  TS5  |  NSM  |  Z11  |  Z21  |def  |
|+ef     |  Z13  |  Z23  |  Z15  |  Z25  |       |       |       |       |     |
|PTRSHL  |   10  |   20  |   3.0 |   6.0 |   4.0 |   30  |  2.25 |  18.0 |PQR  |
|+QR     |  5.33 |   40  |  2.5  |  5.0  |  3.5  |  50.0 |  1.5  | -1.5  |STU  |
|+TU     |  3.0  | -3.0  |  2.0  | -2.0  |       |       |       |       |     |

Field       Contents

PID         Property identification number (Integer > 0).

MID1        Material identification number for membrane (Integer > 0).

T1, T3, T5  Thickness at vertices 1, 3, and 5 of the element, respectively
            (Real >= 0.0).

MID2        Material identification number for bending (Integer > 0).

I1, I3, I5  Area moments of inertia per unit width at the vertices 1, 3, and 5
            of the element, respectively (Real >= 0.0).

MID3        Material identification number for transverse shear (Integer >=
            0).

TS1, TS3, TS5  Transverse shear thickness at the vertices 1, 3, and 5 of the 
            element,respectively (Real >= 0.0).

NSM         Nonstructural mass per unit area (Real).

Z11, Z21, Z13, Z23, Z15, Z25  Fiber distances for stress computation at grid 
            points G1, G3, and G5 respectively, positive according to the
            right-hand sequence defined on the CTRSHL card (Real >= 0.0).

## Remarks

1. All PTRSHL cards must have unique property identification numbers.

2. If T3 or T5 are equal to 0.0 or blank, they will be set equal to T1.

3. If I3 or I5 are equal to 0.0  or blank, they will be set equal to I1.

4. If TS3 or TS5 are equal to 0.0 or blank, they will be set equal to TS1.

5. If TS1 is 0.0 or blank, the element is assumed to be rigid in transverse
   shear.

6. The stresses at the centroid will be computed at the top and bottom fibers.
   The stresses at G1, G3, and G5 will be computed at the locations defined on
   the property card (if given).

7. Both continuation cards are required, even if blank.


PTUBE - Tube Property
=====================

## Description

Defines the properties of a thin-walled cylindrical tube element. Referenced
by the CTUBE card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTUBE   |  PID  |  MID  |   OD  |   T   |  NSM  |       |       |       |     |
|PTUBE   |   2   |   6   |  6.29 |  0.25 |       |       |       |       |     |

Field      Contents
-----      --------
PID        Property identification number (Integer > 0).

MID        Material identification number (Integer > 0).

OD         Outside diameter of tube (Real > 0.0).

T          Thickness of tube (Real; T <= 1/2 OD).

NSM        Nonstructural mass per unit length (Real).

## Remarks

1. If T is zero, a solid circular rod is assumed.

2. PTUBE cards must all have unique property identification numbers.

3. For structural problems, PTUBE cards may only reference MAT1 material
   cards.

4. For heat transfer problems, PTUBE cards may only reference MAT4 or MAT5
   material cards.


PTWIST - Twist Panel Property
=============================

## Description

Defines the elastic properties of a twist panel element. Referenced by the
CTWIST card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PTWIST  |  PID  |  MID  |   T   |  NSM  |  PID  |  MID  |   T   |  NSM  |     |
|PTWIST  |   4   |   6   |  2.3  |  9.4  |   5   |   6   |  1.6  |       |     |

Field      Contents
-----      --------
PID        Property identification number (Integer > 0).

MID        Material identification number (Integer > 0).

T          Thickness of twist panel (Real not equal 0.0).

NSM        Nonstructural mass per unit area (Real).

## Remarks

1. All PTWIST cards must have unique identification numbers.

2. PTWIST cards may only reference MAT1 material cards.

3. One or two twist panel properties may be defined on a single card.


PVISC - Viscous Element Property
================================

## Description

Defines the viscous properties of a one-dimensional viscous element which is
used to create viscous elements by means of the CVISC card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|PVISC   |  PID  |   C1  |   C2  |       |  PID  |   C1  |   C2  |       |     |
|PVISC   |   3   |  6.2  |  3.94 |       |       |       |       |       |     |

Field      Contents
-----      --------
PID        Property identification number (Integer > 0).

C1, C2     Viscous coefficients for extension and rotation (Real).

## Remarks

1. This card is used for both extensional and rotational viscous elements.

2. This card has meaning for dynamics problems only.

3. Viscous properties are material independent; in particular, they are
   temperature-independent.

4. One or two viscous element properties may be defined on a single card.

5. This card is used only for direct formulation of dynamic analyses.


QBDY1 - Boundary Heat Flux Load
===============================

## Description

Defines a uniform heat flux into HBDY elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|QBDY1   |  SID  |  Q0   |  EID  |  EID  |  EIDm |"THRU" |  EIDn |  EID  |abc  |
|+bc     |  EID  |  etc. |       |       |       |       |       |       |def  |
|QBDY1   |  109  | 1.-5  |  721  |  723  |  731  | THRU  |  790  |  796  |ABC  |
|+BC     |  801  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

Q0         Heat flux into element (Real).

EID, EIDm, EIDn  HBDY elements (Integer > 0; EIDm < EIDn).

## Remarks

1. QBDY1 cards must be selected in the Case Control Deck (LOAD = SID) to be
   used in statics. The power contributed into an element via this card is
   given by the equation:

   P   = [(Effective area)*Q0+A] *F(t-ç)
    in

   where effective area is taken from PHBDY cards and A is taken from DAREA
   card.

2. QBDY1 cards must be referenced on a TLOADi card for use in transient
   analysis. The power contributed into an element via this card is given by
   the equation:

   P  (t) = [(Effective area)*Q0] *F(t-ç)
    in

   where the function of time, F(t-ç), is specified on a TLOAD or TLOAD2 card.

3. Q0 is positive for heat input.

4. EID may be specified as individual references or as sequential lists (THRU
   sequences) and the two methods may be used interchangeably. The only
   restriction is that integer values must appear in fields 4 and 9 of the
   QBDY1 card and in fields 2 and 9 of each continuation card (if all fields
   are used).


QBDY2 - Boundary Heat Flux Load
===============================

## Description

Defines grid point heat flux into an HBDY element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|QBDY2   |  SID  |  EID  |  Q01  |  Q02  |  Q03  |  Q04  |       |       |     |
|QBDY2   |  109  |  721  | 1.-5  | 1.-5  | 2.-5  | 2.-5  |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

EID        Identification number of an HBDY element (Integer > 0).

Q0i        Heat flux at the ith grid point on the referenced HBDY element
           (Real or blank).

## Remarks

1. QBDY2 cards must be selected in the Case Control Deck (LOAD = SID) to be
   used in statics. The power contributed into each point, i, on an element
   via this card is given by

   P   =  AREA * Q0
    i         i    i

2. QBDY2 cards must be referenced on a TLOAD card for use in transient
   analysis. All connected grid points will have the same time function, but
   may have individual delays. The power contributed into each point, i, or an
   element via this card is given by

   P (t)  =  AREA  * Q0 * F(t-ç )
    i            i     i       i

   where F(t-çi) is a function of time specified on a TLOAD1 or TLOAD2 card.

3. Q0i is positive for heat flux input to the element.


QHBDY - Boundary Heat Flux Load
===============================

## Description

Defines a uniform heat flux into a set of grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|QHBDY   |  SID  |  FLAG |   Q0  |   AF  |   G1  |   G2  |   G3  |   G4  |     |
|QHBDY   |  120  |  LINE | 1.5+3 |  .75  |   13  |   15  |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

FLAG       Type of area involved. Must be one of the following: POINT, LINE,
           REV, AREA3, or AREA4.

Q0         Heat flux into an area (Real).

AF         Area factor depends on type (Real > 0.0 or blank).

G1,...,G4  Grid point identification of connected points (Integer > 0 or
           blank).

## Remarks

1. The heat flux applied to the area is transformed to loads on the points.
   These points need not correspond to an HBDY element.

2. The flux is applied to each point, i, by the equation

   P  =  AREA  * Q0
    i        i

   where Q0 is positive for heat input, and AREAi is the portion of the total
   area associated with point i.

3. In statics, the load is applied with the Case Control request: LOAD = SID.
   In dynamics, the load is applied by reference on a TLOADi data card. The
   load at each point will be multiplied by the function of time F(t-çi)
   defined on the TLOADi card. çi is the delay factor for each point.

4. The number of connected points for the five types are 1(POINT),
   2(LINE,REV), 3(AREA3), 4(AREA4). Any unused Gi entries must be on the
   right.

5. The area factor AF is used to determine the effective area for the POINT
   and LINE types. It equals the area and the effective width, respectively.
   It is ignored for the other types, which have their area defined
   implicitly.

6. The type flag defines a surface in the same manner as the CHBDY data card.
   For physical descriptions of the geometry involved, see the CHBDY
   description.


QVECT - Thermal Flux Vector Load
================================

## Description

Defines thermal flux vector from a distant source into HBDY elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|QVECT   |  SID  |  Q0   |  E1   |  E2   |  E3   |  EID1 |  EID2 |  EID3 |abc  |
|+bc     |  EID4 |  EID5 |  etc. |       |       |       |       |       |def  |
|QVECT   |  333  | 1.-2  | -1.0  |  0.0  |  0.0  |  721  |  722  |  723  |ABC  |
|+BC     |  724  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

Q0         Magnitude of thermal flux vector (Real).

E1, E2, E3 Vector components (in basic coordinate system) of the thermal flux
           vector (Real or Integer > 0). The total flux is given by Q =
           Q0{E1,E2,E3}.

EIDi       Element identification numbers of HBDY elements irradiated by the
           distant source (Integer > 0).

## Remarks

1. For statics, the load set is selected in the Case Control Deck (LOAD =
   SID). The power contributed into an element via this card is given by

              _   _
   P   =  -+A(e * n) * Q0
    in

   where:

   +   =  absorbtivity
   A   =  area of HBDY element
   _
   e   =  vector of real numbers E1, E2, E3
   _
   n   =  positive normal vector of element, see CHBDY data
          card description
    _   _
   (e * n)   =  0 if the vector product is positive (that is, the flux
                is coming from behind the element)

2. For transient analysis, the load set (SID) is selected by a TLOADi card
   which defines a load function of time. The power contributed into the
   element via this card is given by

                 _    _
   P (t)  =  -+A(e(t)*n)*Q0*F(t-ç)
    l

   where:

            _
   +,A, and n are the same as the statics case
   _
   e(t) =   vector of three functions of time, which may be
            given on TABLEDi data cards. If E1, E2, or E3 is
            an integer, it is the table identification
            number. If E1, E2, or E3 is a real number,
            its value is used directly; if Ei is blank, its
            value is zero.

   F(t-ç) is a function of time specified or referenced by
            the parent TLOAD1 or TLOAD2 card. The value ç is
            calculated for each loaded point.

3. If the referenced HBDY element is of TYPE = ELCYL, the power input is an
   exact integration over the area exposed to the thermal flux vector.

4. If the referenced HBDY element is of TYPE = REV, the vector should be
   parallel to the basic z axis.

5. If a sequential list of elements is desired, fields 7, 8, and 9 may specify
   the first element, the BCD string THRU, and the last element. No subsequent
   data is allowed with this option.


QVOL - Volume Heat Addition
===========================

## Description

Defines a rate of internal heat generation in an element.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|QVOL    |  SID  |  QV   |  EID  |  EID  |  EIDm |"THRU" |  EIDn |  EID  |abc  |
|+bc     |  EID7 |  etc. |       |       |       |       |       |       |def  |
|QVOL    |  333  | 1.+2  |  301  |  303  |  317  | THRU  |  345  |  416  |ABC  |
|+BC     |  527  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

QV         Power input per unit volume produced by a heat conduction element
           (Real).

EID, EIDm, EIDn  Heat conduction element identification numbers (Integer > 0;
           EIDm < EIDn).

## Remarks

1. In statics, the load is applied with the Case Control request, LOAD = SID.
   The equivalent power contributed via this card into each grid point, i,
   connected to each element listed, is given by

   P  = QV * VOL
    i           i

   where VOLi is the portion of the volume associated with point i and QV is
   positive for heat generation.

2. In dynamics, the load is requested by reference on a TLOADi card. The
   equivalent power contributed via this card into each grid point i,
   connected to each element listed, is

   P  = QV * VOL * F(t-ç )
    i           i       i

   where VOLi is the portion of the volume associated with point i and F(t-çi)
   is the function of time defined by a TLOADi card. çi is the delay for each
   point i.

3. EID may be specified as individual references or as sequential lists (THRU
   sequences) and the two forms may be used interchangeably. The only
   restriction is that integer values must appear in fields 4 and 9 of the
   QVOL card and in fields 2 and 9 of each continuation card (if all fields
   are used).


RADLST - List of Radiation Areas
================================

## Description

A list of HBDY identification numbers given in the same order as the columns
of the RADMTX matrix.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RADLST  |  EID1 |  EID2 |  EID3 |  EID4 |  EID5 |  EID6 |  EID7 |  EID8 |abc  |
|+bc     |  EID9 |  etc. |       |       |       |       |       |       |def  |
|RADLST  |   10  |   20  |   30  |   50  |   31  |   41  | THRU  |   61  |ABC  |
|+BC     |   71  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EIDi       The element identification numbers of the HBDY elements, given in
           the order that they appear in the RADMTX matrix (Integer > 0 or
           BCD THRU).

## Remarks

1. This card is required if a RADMTX is defined.

2. Only one RADLST card string is allowed in a data deck.

3. If a group of the elements are sequential, any field except 2 and 9 may
   contain the BCD word THRU. Element ID numbers will be generated for every
   integer between the value of the previous field and the value of the
   subsequent field. The values must increase, however.

4. Any element may be listed more than once. For instance, if both sides of a
   panel are radiating, each side may participate in a different part of the
   view factor matrix.


RADMTX - Radiation Matrix
=========================

## Description

Matrix of radiation exchange coefficients (area times view factor) for
nonlinear heat transfer analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RADMTX  | INDEX |  Fi,i | Fi+1,i| Fi+2,i| Fi+3,i| Fi+4,i| Fi+5,i| Fi+6,i|abc  |
|+bc     | Fi+7,i|  etc. |       |       |       |       |       |       |def  |
|RADMTX  |   3   |   0.  |  9.3  |  17.2 |  16.1 |  .1   |   0.  |  6.2  |ABC  |
|+BC     |  6.2  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
INDEX      The column number of the matrix (Integer > 0).

Fi+k,i     The matrix values (Real), starting on the diagonal, continuing
           down the column. A group of zeros at the bottom of the column may
           be omitted. A blank field will end the column, which disallows
           imbedded blank fields.

## Remarks

1. The INDEX numbers go from 1 through NA, where NA is the number of radiating
   areas.

2. The radiation exchange coefficient matrix is symmetric, and only the lower
   triangle is input. Column 1 is associated with the HBDY element first
   listed on the RADLST card, Column 2 for the next, etc. Null columns need
   not be entered.

        NA
3. P  = -  F   q
    i  j=1  ij  j

   Pi = total irradiation into element i

   qj = radiosity (per unit area) at j

   Fij = radiation matrix (units of area)

4. A column may only be specified once.

5. An element identification appearing on a RADLIST card that is not defined
   on a RADMTX card or is only partially defined, will cause the missing terms
   of the matrix column to be filled with zeros. This implies an infinite heat
   sink (radiation loss) is present.


RANDPS - Power Spectral Density Specification
=============================================

## Description

Defines load set power spectral density factors for use in random analysis
having the frequency dependent form

   S  (F)  =  (X + iY) G(F)
    jk

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RANDPS  |  SID  |   J   |   K   |   X   |   Y   |  TID  |       |       |     |
|RANDPS  |   5   |   3   |   7   |  2.0  |  2.5  |   4   |       |       |     |

Field      Contents
-----      --------
SID        Random analysis set identification number (Integer > 0).

J          Subcase identification number of excited load set (Integer > 0).

K          Subcase identification number of applied load set (Integer >= 0; K
           >= J).

X, Y       Components of complex number (Real).

TID        Identification number of a TABRNDi card which defines G(F)
           (Integer >= 0).

## Remarks

1. If J = K, then Y must be 0.0.

2. For TID = 0, G(F) = 1.0.

3. Set identification numbers must be selected in the Case Control Deck
   (RANDOM = SID) to be used by NASTRAN.

4. Only 20 unique sets may be defined. However, as many RANDPS cards as
   desired with the same SID may be input.

5. RANDPS can only reference subcases included within a single loop (change in
   direct matrix input is not allowed).

6. Subcase number must be specified in the Case Control Deck.


# RANDT1 - Autocorrelation Function Time Lag

## Description

Defines time lag constants for use in random analysis autocorrelation function
computation.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RANDT1  |  SID  |   N   |   T0  |  TMAX |       |       |       |       |     |
|RANDT1  |   5   |   10  |  3.2  |  9.6  |       |       |       |       |     |

Field      Contents
-----      --------
SID        Random analysis set identification number (Integer > 0).

N          Number of time lag intervals (Integer > 0).

T0         Starting time lag (Real >= 0.0).

TMAX       Maximum time lag (Real > T0).

## Remarks

1. At least one RANDPS card must be present with the same set identification
   number.

2. The time lags defined on this card are given by

                T    -  T
                 max     o
   T   =  T  + ----------- (i - 1),      i = 1, N + 1
    i      o        N

3. Time lag sets must be selected in the Case Control Deck (RANDOM = SID) to
   be used by NASTRAN.

RELES - Release Substructure Connectivities
===========================================

## Description

Defines sets of component degrees of freedom at substructure grid points which
are not to be connected.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RELES   |  SID  |  NAME |   G1  |   C1  |   G2  |   C2  |   G3  |   C3  |def  |
|+ef     |   G4  |   C4  |      etc.     |   GN  |   CN  |       |       |     |
|RELESL  |   6   |WINGRT |   17  |  456  |   18  |  456  |   21  |  123  |DEF  |
|+EF     |   253 |  456  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

NAME       Name of basic substructure (BCD).

Gi         Grid or scalar point identification number (Integer > 0).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

## Remarks

1. The RELES data will override any connections generated automatically from
   geometry and any connections defined on CONCT data cards.

2. The RELES data will not override connections defined on the CONCT1 data
   card.

3. Connectivity sets must be selected in the Substructure Control Deck
   (CONNECT = SID) to be used by NASTRAN. Note that CONNECT is a subcommand of
   the substructure COMBINE command.

4. Connectivities defined during previously executed COMBINE operations will
   be retained and may be referenced by the grid point ID and component of any
   one of the basic substructures associated with that connectivity.


REMFLUX - Remanent Flux Density
===============================

## Description

Specifies remanent flux density for selected elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|REMFLUX |  SID  |  CID  |  BRX  |  BRY  |  BRZ  | EID1  | EID2  | EID3  |     |
|REMFLUX |   2   |       |   1.  |   2.  |   3.  |   1   |   2   |   3   |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|REMFLUX |  SID  |  CID  |  BRX  |  BRY  |  BRZ  | EID1  |"THRU" | EID2  |     |
|REMFLUX |   2   |       |   1.  |   2.  |   3.  |   1   | THRU  |   3   |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

CID        Coordinate system identification number (Integer > 0).

BRX, BRY, BRZ  Remanent flux density in coordinate system CID (Real).

EID1, EID2, EID3  Element identification numbers (Integer > 0).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. If the alternate form of the card is used, all elements between EID1 and
   EID2 need not exist, but sufficient core must be available for 5 words
   (EID2 - EID1 + 1).

3. REMFLUX cards may not have the same load set identification number as
   SPCFLD, CEMLOOP, GEMLOOP, or MDIPOLE cards. However, they may be combined
   on a LOAD card or a SUBCOM card.

4. CID must presently be 0 or blank.


RFORCE - Rotational Force
=========================

## Description

Defines a static loading condition due to a centrifugal force field.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RFORCE  |  SID  |   G   |  CID  |   A   |   N1  |   N2  |   N3  |       |     |
|RFORCE  |   2   |   5   |       | -6.4  |  0.0  |  0.0  |  1.0  |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

G          Grid point identification number (Integer > 0).

CID        Coordinate system defining rotation direction (Integer >= 0 or
           blank).

A          Scale factor for rotational velocity in revolutions per unit time
           (Real).

N1, N2, N3 Rectangular components of rotation direction vector (Real; N1**2 +
           N2**2 + N3**2 > 0.0) The vector defined will act at point G.

## Remarks

1. G = 0 means the basic coordinate system origin.

2. CID = 0 means the basic coordinate system.

3. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

4. Rotational force sets can be combined with other static loads only by using
   the LOAD bulk data card.

5. The load vector generated by this card can be printed with an OLOAD request
   in the Case Control Deck.

6. For elements with lumped mass, the centrifugal acceleration is calculated
   at the center of the lumped mass. Grid point offsets of the mass such as
   those defined with BAR and CONM2 elements are taken into account.

7. For elements using the coupled consistent mass option (COUPMASS) or those
   with implicit coupled mass matrices such as IHEXi and TRIAAX elements, the
   centrifugal accelerations are calculated based on grid point locations.
   This acceleration vector is then multiplied by the mass matrix to generate
   loads. Therefore, for greater accuracy, elements near the axis of rotation
   should be kept small to best represent the actual acceleration field.

8. When applying a rotational force to an axisymmetric element, G and CID must
   be 0 or blank; N1 and N2 must be 0.0.


RINGAX - Axisymmetric Ring
==========================

## Description

Defines a ring for a model containing CCONEAX, CTRAPAX, or CTRIAAX elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RINGAX  |   ID  |       |   R   |   Z   |       |       |   PS  |       |     |
|RINGAX  |   3   |       |  2.0  | -10.0 |       |       |  162  |       |     |

Field      Contents
-----      --------
ID         Ring identification number (1 <= Integer < 10**6).

R          Ring radius (Real > 0.0).

Z          Ring axial location (Real).

PS         Permanent single-point constraints (any unique combination of the
           digits 1 - 6).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. The number of degrees of freedom defined is (6-PS)*H where H is the
   harmonic count and PS is the number of digits in field 8. (See AXIC card.)

3. RINGAX identification numbers must be unique with respect to all other
   POINTAX, RINGAX, and SECTAX identification numbers.

4. The fourth and sixth degrees of freedom must be constrained when transverse
   shear flexibility is not included for the conical shell.

5. For a discussion of the conical shell problem see Section 5.9 of the
   Theoretical Manual.

6. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


RINGFL - Axisymmetric Fluid Point
=================================

## Description

Defines a circle (fluid point) in an axisymmetric fluid model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RINGFL  |  IDF  |   X1  |   X2  |   X3  |  IDF  |   X1  |   X2  |   X3  |     |
|RINGFL  |   3   |  1.0  |       |  30.0 |       |       |       |       |     |

Field      Contents
-----      --------
IDF        Unique identification number of the fluid point (Integer, 0 < IDF
           < 10**5).

X1, X2, X3 Coordinates of point in fluid coordinate system defined on AXIF
           card (Real; X1 > 0.0).

## Remarks

1. This card is allowed only if an AXIF card is also present.

2. All fluid point identification numbers must be unique with respect to other
   scalar, structural and fluid points.

3. X1, X2, X3 are (r, í, z) for a cylindrical coordinate system and (p, é, í)
   for a spherical coordinate system. é and í are in degrees. The value of é
   must be greater than zero. The value of í must be blank or zero.

4. One or two fluid points may be defined per card.


RLOAD1 - Frequency Response Dynamic Load
========================================

## Description

Defines a frequency dependent dynamic load of the form

                              i{é - 2+fç}
   {P(f)} =  A[C(f) + iD(f)] e

for use in frequency response problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RLOAD1  |  SID  |   L   |   M   |   N   |   TC  |   TD  |       |       |     |
|RLOAD1  |   5   |   3   |   6   |   9   |   1   |   2   |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

L          Identification number of DAREA or DAREAS and LOADC card set which
           defines A (Integer > 0).

M          Identification number of DELAY or DELAYS card set which defines ç
           (Integer >= 0).

N          Identification number of DPHASE or DPHASES card set which defines
           é (Integer >= 0).

TC         Set identification number of TABLEDi card which gives C(f)
           (Integer >= 0; TC + TD > 0).

TD         Set identification number of TABLEDi card which gives D(f)
           (Integer >= 0; TC + TD > 0).

## Remarks

1. If any of M, N, TC, or TD are blank or zero, the corresponding ç, é, C(f),
   or D(f) will be zero.

2. Dynamic load sets must be selected in the Case Control Deck (DLOAD = SID)
   to be used by NASTRAN.

3. RLOAD1 loads may be combined with RLOAD2 loads only by specification on a
   DLOAD card. That is, the SID on an RLOAD1 card may not be the same as that
   on an RLOAD2 card.

4. SID must be unique for all RLOAD1, RLOAD2, TLOAD1, and TLOAD2 cards.

5. With automated multi-stage substructuring, DAREAS cards may only reference
   degrees of freedom in the boundary set of the solution structure.

6. When L references LOADC cards, DAREAS cards with the same set
   identification and non-zero loads must also exist.


RLOAD2 - Frequency Response Dynamic Load
========================================

## Description

Defines a frequency dependent dynamic load of the form

                    i{í(f) + é  - 2+fç}
   {P(f)}  =  AB(f)e

for use in frequency response problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|RLOAD2  |  SID  |   L   |   M   |   N   |   TB  |   TP  |       |       |     |
|RLOAD2  |   5   |   3   |   6   |   21  |   7   |   2   |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

L          Identification number of DAREA or DAREAS and LOADC card set which
           defines A (Integer > 0).

M          Identification number of DELAY or DELAYS card set which defines ç
           (Integer >= 0).

N          Identification number of DPHASE or DPHASES card set which defines
           é in degrees (Integer >= 0).

TB         Set identification number of TABLEDi card which gives B(f)
           (Integer >= 0).

TP         Set identification number of TABLEDi card which gives í(f) in
           degrees (Integer >= 0).

## Remarks

1. If any of M, N, or TP are zero, the corresponding ç, é, or í(f) will be
   zero.

2. Dynamic load sets must be selected in the Case Control Deck (DLOAD = SID)
   to be used by NASTRAN.

3. RLOAD2 loads may be combined with RLOAD1 loads only by specification on a
   DLOAD card. That is, the SID on an RLOAD2 card may not be the same as that
   on an RLOAD1 card.

4. SID must be unique for all RLOAD1, RLOAD2, TLOAD1, and TLOAD2 cards.

5. With automated multi-stage substructuring, DAREAS cards may only reference
   degrees of freedom in the boundary set of the solution structure.

6. When L references LOADC cards, DAREAS cards with the same set
   identification and non-zero loads must also exist.


SECTAX - Axisymmetric Sector
============================

## Description

Defines a sector of a model containing CCONEAX, CTRAPAX, or CTRIAAX elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SECTAX  |   ID  |  RID  |   R   |  PHI1 |  PHI2 |       |       |       |     |
|SECTAX  |   1   |   2   |  3.0  |  30.0 |  40.0 |       |       |       |     |

Field      Contents
-----      --------
ID         Sector identification number (unique Integer > 0).

RID        Ring identification number (see RINGAX) (Integer > 0).

R          Effective radius (Real).

PHI1, PHI2 Azimuthal limits of sector in degrees (Real).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. SECTAX identification numbers must be unique with respect to all other
   POINTAX, RINGAX, and SECTAX identification numbers.

3. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

4. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


SEQEP - Extra Point Resequencing
================================

## Description

The purpose of the SEQEP card is to allow re-identifying the formation
sequence of the extra points of his structural model in such a way as to
optimize bandwidth, which is essential for efficient solutions by the
displacement method.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SEQEP   |   ID  | SEQID |   ID  | SEQID |   ID  | SEQID |   ID  | SEQID |     |
|SEQEP   |  5392 |  15.6 |       |       |   2   |1.9.2.6|   3   |   2   |     |

Field      Contents
-----      --------
ID         Extra point identification number (Integer > 0).

SEQID      Sequence identification number (a special number described below).

## Remarks

1. ID is any extra point identification number which is to be re-identified
   for sequencing purposes. The sequence number is a special number which may
   have any of the following forms where X is a decimal integer digit:
   XXXX.X.X.X, XXXX.X.X, XXXX.X, or XXXX, where any of the leading X's may be
   omitted. This number must contain no imbedded blanks.

2. To insert an extra point between two already existing grid, scalar, and/or
   extra points, such as 15 and 16, for example, define it as, say 5392, and
   then use this card to insert extra point number 5392 between them by
   equivalencing it to, say, 15.6. All output referencing this point will
   refer to 5392.

3. The SEQID numbers must be unique and may not be the same as a point ID
   which is not being changed. No extra point ID may be referenced more than
   once.

4. No continuation cards (small field or large field) are allowed with either
   the SEQGP or the SEQEP card.

5. From one to four extra points may be resequenced on a single card.


SEQGP - Grid and Scalar Point Resequencing
==========================================

## Description

Used to order the grid points and user-supplied scalar points of the problem.
The purpose of this card is to allow re-identifying the formation sequence of
the grid and scalar points of his structural model in such a way as to
optimize bandwidth, which is essential for efficient solutions by the
displacement method.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SEQGP   |   ID  | SEQID |   ID  | SEQID |   ID  | SEQID |   ID  | SEQID |     |
|SEQGP   |  5392 | 15.6  |       |       |   2   |1.9.2.6|   3   |   2   |     |

Field      Contents
-----      --------
ID         Grid or scalar point identification number (Integer > 0).

SEQID      Sequenced identification number (a special number described
           below).

## Remarks

1. ID is any grid or scalar point identification number which is to be
   re-identified for sequencing purposes. The grid point sequence number
   (SEQID) is a special number which may have any of the following forms where
   X is a decimal integer digit: XXXX.X.X.X, XXXX.X.X, XXXX.X, or XXXX, where
   any of the leading X's may be omitted. This number must contain mo imbedded
   blanks.

2. To insert a grid point between two already existing grid points, such as 15
   and 16, for example, define it as, say 5392, and then use this card to
   insert grid point number 5392 between them by equivalencing it to, say
   15.6. All output referencing this point will refer to 5392.

3. The SEQID numbers must be unique and may not be the same as a point ID
which is not being changed. No grid point ID may be referenced more than
once.

4. No continuation cards (small field or large field) are allowed with either
   the SEQGP or the SEQEP card.

5. From one to four grid or scalar points may be resequenced on a single card.

6. SEQGP is not available for axisymmetric and hydroelastic problems.


SET1 - Grid Point List
======================

## Description

Defines a set of structural grid points by a list.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SET1    |  SID  |   G1  |   G2  |   G3  |   G4  |   G5  |   G6  |   G7  |ABC  |
|+BC     |   G8  |  etc. |       |       |       |       |       |       |     |
|SET1    |   3   |   31  |   62  |   93  |  124  |   16  |   17  |   18  |ABC  |
|+BC     |   19  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set of identification numbers (Integer > 0).

G1, G2, etc.  List of structural grid points (Integer > 0 or THRU).

## Remarks

1. These cards are referenced by the SPLINE data cards.

2. When using the THRU option, all intermediate grid points must exist. The
   word THRU may not appear in field 3 or 9 (2 or 9 for continuation cards.)


SET2 - Grid Point List
======================

## Description

Defines a set of structural grid points in terms of aerodynamic macro
elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SET2    |  SID  | MACRO |  SP1  |  SP2  |  CH1  |  CH2  |  ZMAX |  ZMIN |     |
|SET2    |   3   |  111  |   .0  |  .75  |   .0  | .667  |  1.0  | -3.51 |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

MACRO      Element identification number of an aero macro element (Integer >
           0).

SP1, SP2   Lower and higher span division points defining prism containing
           set (-.01 < Real < 1.01)

CH1, CH2   Lower and higher chord division points defining prism containing
           set (-.01 < Real < 1.01)

ZMAX, ZMIN Top and bottom z coordinates (using right-hand rule with the order
           the corners are listed on a CAERO1 card) of the prism containing
           set (Real). Usually ZMAX >= 0, ZMIN <= 0.

## Remarks

1. These cards are referenced by the SPLINEi data cards.

2. Every grid point within the defined prism and within the height range will
   be in the set. For example,

               CH1 = 0.0           MACRO 111
             +----------+----------+----------+----------+
             |//////////|//////////|//////////|          |
             |///111////|///114////|///117////|   120    |
             |//////////|//////////|//////////|          |
             +----------+----------+----------+----------+
             |//////////|//////////|//////////|          |
   SP1 = 0.0 |///112////|///115////|///118////|   121    | SP2 = 0.75
             |//////////|//////////|//////////|          |
             +----------+----------+----------+----------+
             |          |          |          |          |
             |   113    |   116    |   119    |   122    |
             |          |          |          |          |
             +----------+----------+----------+----------+
               CH2 = .667

   The shaded area in the figure defines the cross-section of the prism for
   the sample data given above. Points exactly on the boundary may be missed;
   hence, to get the area of the macro element, use SP1 = -.01, SP2 = 1.01,
   etc.

3. A zero value for ZMAX or ZMIN implies infinity is to be used.

4. To find the (internal) grid ID's found, use DIAG 18.


SLBDY - Slot Boundary List
==========================

## Description

Defines a list of slot points which lie on an interface between an
axisymmetric fluid and a set of evenly spaced radial slots.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SLBDY   |  RHO  |   M   |  ID1  |  ID2  |  ID3  |  ID4  |  ID5  |  ID6  |abc  |
|+bc     |  ID7  |  etc. |       |       |       |       |       |       |+def |
|SLBDY   | 0.002 |   6   |   16  |   17  |   18  |   25  |   20  |   21  |+BDY |
|+BDY    |   22  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
RHO        Density of fluid at boundary (Real > 0.0, or blank).

M          Number of slots (Integer >= 0, or blank).

IDj        Identification numbers of GRIDS slot points at boundary with
           axisymmetric fluid cavity, j = 1,2,...,J (Integer > 0).

## Remarks

1. This card is allowed only if an AXSLOT card is also present.

2. If RHO or M is blank the default value on the AXSLOT card is used. The
   effective value must not be zero for RHO. If the effective value of M is
   zero, no matrices at the boundary will be generated.

3. The order of the list of points determines the topology of the boundary.
   The points are listed sequentially as one travels along the boundary in
   either direction. At least two points must be defined.

4. More than one logical boundary card may be used.


SLOAD - Static Scalar Load
==========================

## Description

Used to apply static loads to scalar points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SLOAD   |  SID  |   S   |   F   |   S   |   F   |   S   |   F   |       |     |
|SLOAD   |   16  |   2   |  5.9  |   17  | -6.3  |   14  | -2.93 |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

S          Scalar point identification number (Integer > 0).

F          Load value (Real).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. Up to three scalar loads may be defined on a single card.

SPC - Single-Point Constraint

## Description

Defines sets of single-point constraints and enforced displacements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPC     |  SID  |   G   |   C   |   D   |   G   |   C   |   D   |       |     |
|SPC     |   2   |   32  |  436  | -2.6  |   5   |       | +2.9  |       |     |

Field      Contents
-----      --------
SID        Identification number of single-point constraint set (Integer >
           0).

G          Grid or scalar point identification number (Integer > 0).

C          Component number (any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when point identification numbers are grid
           points; zero or blank if point identification numbers are scalar
           points).

D          Value of enforced displacement for all coordinates designated by G
           and C (Real).

## Remarks

1. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multipoint constraint relation (MPC card) or as a degree of
   freedom on a rigid element (CRIGD1, CRIGD2, CRIGD3, CRIGDR), nor may it be
   referenced on a SPC1, OMIT, OMIT1, or SUPORT card. D must be 0.0 for
   dynamics problems.

2. Single-point forces of constraint are recovered during stress data
   recovery.

3. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

4. From one to twelve single-point constraints may be defined on a single
   card.

5. SPC degrees of freedom may be redundantly specified as permanent
   constraints on the GRID card.

6. The enforced displacement, D, is used only in static analyses (Rigid
   Formats 1, 2, 4, 5, 6, 14).

7. In heat transfer analysis, constraints applied to component number 1 are
   used to fix the temperature at that point.

8. D may be used to define an enforced temperature in static heat transfer
   analysis (Rigid Format 1 only). See Section 1.8 for methods of defining
   boundary temperatures in other Rigid Formats.

SPC1 - Single-Point Constraint
==============================

## Description

Defines sets of single-point constraints.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPC1    |  SID  |   C   |   G1  |   G2  |   G3  |   G4  |   G5  |   G6  |abc  |
|+bc     |   G7  |   G8  |   G9  |  etc. |       |       |       |       |     |
|SPC1    |   3   |   2   |   1   |   3   |   10  |   9   |   6   |   5   |ABC  |
|+BC     |   2   |   8   |       |       |       |       |       |       |     |

Alternate Form
--------------

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPC1    |  SID  |   C   |  GID1 |"THRU" |  GID2 |       |       |       |     |
|SPC1    |  313  | 12456 |   6   | THRU  |   32  |       |       |       |     |

Field      Contents
-----      --------
SID        Identification number of single-point constraint set (Integer >
           0).

C          Component number (any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when point identification numbers are grid
           points; zero or blank if point identification numbers are scalar
           points).

Gi, GIDi   Grid or scalar point identification numbers (Integer > 0).

## Remarks

1. Note that enforced displacements are not available via this card. As many
   continuation cards as desired may appear when THRU is not used.

2. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multi-point constraint relation (MPC) or as a degree of
   freedom on a rigid element (CRIGD1, CRIGD2, CRIGD3, CRIGDR), nor may it be
   referenced on a SPC, OMIT, OMIT1, or SUPORT card.

3. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

4. SPC degrees of freedom may be redundantly specified as permanent
   constraints on the GRID card.

5. All grid points referenced by GID1 through GID2 must exist.

6. In heat transfer analysis, constraints applied to component number 1 are
   used to fix the temperature at a point.

7. C is 1 only for a heat problem.


SPCADD - Single-Point Constraint
================================

## Description

Defines a single-point constraint set as a union of single-point constraint
sets defined via SPC or SPC1 cards.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCADD  |  SID  |   S1  |   S2  |   S3  |   S4  |   S5  |   S6  |   S7  |abc  |
|+bc     |   S8  |   S9  |     etc.      |       |       |       |       |     |
|SPCADD  |  100  |   3   |   2   |   9   |   1   |       |       |       |     |
|+BC     |       |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Identification number for new single-point constraint set (Integer
           > 0; not equal 101 or 102 if axisymmetric).

Si         Identification numbers of single-point constraint sets defined via
           SPC or SPC1 cards (Integer > 0; SID not equal Si).

## Remarks

1. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

2. No Si may be the identification number of a single-point constraint set
   defined by another SPCADD card.

3. The Si values must be unique.

4. Set identification numbers of 101 or 102 cannot be used in axisymmetric
   problems.


SPCAX - Axisymmetric Single-Point Constraint
============================================

## Description

Defines sets of single-point constraints for a model containing CCONEAX,
CTRAPAX, or CTRIAAX elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCAX   |  SID  |  RID  |  HID  |   C   |   V   |       |       |       |     |
|SPCAX   |   2   |   3   |   4   |   13  |  6.0  |       |       |       |     |

Field      Contents
-----      --------
SID        Identification number of single-point constraint set (Integer > 0;
           not equal 101 or 102).

RID        Ring identification number (see RINGAX) (Integer >= 0).

HID        Harmonic identification number (Integer >= 0).

C          Component identification number (any unique combination of the
           digits 1 - 6).

V          Enforced displacement value (Real).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

3. Coordinates appearing on SPCAX cards may not appear on MPCAX, SUPAX, or
   OMITAX cards.

4. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

5. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


SPCD - Enforced Displacement Value
==================================

## Description

Defines an enforced displacement value for static analysis, which is requested
as a LOAD.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCD    |  SID  |   G   |   C   |   D   |   G   |   C   |   D   |       |     |
|SPCD    |  100  |   32  |  436  | -2.6  |   5   |       | +2.9  |       |     |

Field      Contents
-----      --------
SID        Identification number of a static load set (Integer > 0).

G          Grid or scalar point identification number (Integer > 0).

C          Component number (any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when point identification numbers are grid
           points; zero or blank if point identification numbers are scalar
           points).

D          Value of enforced displacement for all coordinates designated by G
           and C (Real).

## Remarks

1. A coordinate referenced on this card must be referenced by a selected SPC
   or SPC1 data card.

2. Values of D will override the values specified on an SPC bulk data card, if
   the LOAD set is requested.

3. The bulk data LOAD combination card will not request an SPCD.

4. At least one bulk data LOAD card (FORCE, SLOAD, etc.) is required in the
   LOAD set selected in the Case Control Deck.

5. The enforced displacement, D, is used only in static analyses (Rigid
   Formats 1, 2, 4, 5, 6, 14).

6. In heat transfer analysis, D is used to define an enforced temperature in
   statics analysis (Rigid Format 1 only). See Section 1.8 for methods of
   defining boundary temperatures in other Rigid Formats.


SPCFLD - Specified Magnetic Field
=================================

## Description

Specifies magnetic field at selected grid points.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCFLD  |  SID  |  CID  |  HCX  |  HCY  |  HCZ  |   G1  |   G2  |   G3  |     |
|SPCFLD  |   18  |       | 12.25 |   0.  |  62.  |    8  |   17  |   103 |     |

### First Alternate Form:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCFLD  |  SID  |  CID  |  HCX  |  HCY  |  HCZ  |  GID1 |"THRU" |  GID2 |     |
|SPCFLD  |   18  |       | 12.25 |   0.  |  62.  |    9  | THRU  |   27  |     |

### Second Alternate Form:

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCFLD  |   18  |       | 12.25 |   0.  |  62.  |   -1  |       |       |     |

Field      Contents
-----      --------
SID        Load set identification number (Integer > 0).

CID        Coordinate system identification number (Integer > 0 or blank).

HCX, HCY, HCZ  Components of specified Hc field in coordinate system CID
           (Real).

Gi, GIDi   Grid point identification numbers (Integer > 0).

## Remarks

1. Load sets must be selected in the Case Control Deck (LOAD = SID) to be used
   by NASTRAN.

2. If the first alternate form of the card is used, all grid point
   identification numbers between GID1 and GID2 must exist.

3. The second alternate form of the card implies that the specified Hc field
   applies to all grid points.

3. CID must presently be 0 or blank.


SPCS - Substructure Single Point Constraints
============================================

## Description

Defines a set of single point constraints on a specified basic substructure.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCS    |  SID  | NAME  |   G1  |   C1  |   G2  |   C2  |   G3  |   C3  |abc  |
|+bc     |   G4  |   C4  |   G5  |   C5  |   G6  |   C6  |   G7  |   C7  |def  |
|SPCS    |   61  | MIDWG |   9   |   45  |   18  |  124  |   36  |  456  |ABC  |
|+BC     |   88  |  136  |      etc.     |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

NAME       Basic substructure name (BCD).

Gi         Grid or scalar point identification number in substructure
           (Integer > 0).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

## Remarks

1. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multipoint constraint relation, nor may it be referenced on
   a SPCS1, SPC, SP11, OMIT, OMIT1, or SUPORT card.

2. Single-point forces of constraint are recovered during stress data
   recovery.

3. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

4. A single G, C pair may not specify all component degrees of freedom for a
   connected grid point where only some of the degrees of freedom of the grid
   point have been connected or when some have been disconnected via the RELES
   card. The degrees of freedom which were connected and those that were not
   connected must be referenced separately.


SPCS1 - Substructure Single Point Constraints
=============================================

## Description

Defines a set of single point constraints on a specified basic substructure.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCS1   |  SID  | NAME  |   C   |   G1  |   G2  |   G3  |   G4  |   G5  |abc  |
|+bc     |   G6  |   G7  |   G8  |   G9  |  G10  |  G11  |  G12  |  G13  |def  |
|SPCS1   |   15  |FUSELAG|  1236 |  1101 |  1102 |  1105 |  THRU |  1110 |ABC  |
|+BC     |  1121 |  1130 |  THRU |  1140 |  1143 |  1150 |      etc.     |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

NAME       Basic substructure name (BCD).

C          Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

Gi         Grid or scalar point identification numbers (Integer > 0).

## Remarks

1. THRU may appear in fields 6, 7, or 8 of the first card and anywhere in
   fields 3 through 8 on a continuation card.

2. A coordinate referenced on this card may not appear as a dependent
   coordinate in a multipoint constraint relation, nor may it be referenced on
   a SPCS1, SPC, SPC1, OMIT, OMIT1, or SUPORT card.

3. Single-point constraint sets must be selected in the Case Control Deck (SPC
   = SID) to be used by NASTRAN.

4. All grid points referenced by Gi through Gj must exist.

5. A single G, C pair may not specify all component degrees of freedom for a
   connected grid point where only some of the degrees of freedom of the grid
   point have been connected or when some have been disconnected via the RELES
   card. The degrees of freedom which were connected and those that were not
   connected must be referenced separately.


SPCSD - Substructure Enforced Displacement Values
=================================================

## Description

Defines enforced displacement values for a given substructure during static
analysis, which are requested as a LOAD.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPCSD   |  SID  | NAME  |   G1  |   C1  |   D1  |   G2  |   C2  |   D2  |     |
|SPCSD   |   27  |LWINGRT|  965  |   3   |  3.6  |       |       |       |     |

Field      Contents
-----      --------
SlD        Identification number of a static load set (Integer > 0).

NAME       Basic substructure name (BCD).

Gi         Grid or scalar point identification number (Integer > 0).

Ci         Component number; any unique combination of the digits 1 - 6 (with
           no imbedded blanks) when the Gi are grid points, or null if they
           are scalar points.

Di         Value of enforced displacement for all coordinates designated by
           Gi and Ci (Real).

## Remarks

1. A coordinate referenced on this card must be referenced by a selected SPCS
   or SPCS1 data card.

2. The bulk data LOAD combination card will not request an SPCSD.

3. At least one bulk data load card (LOADC or SLOAD) in addition to the SPCSD
   cards is required in the LOAD set selected in case control (LOAD = SID).


SPLINE1 - Surface Spline
========================

## Description

Defines a surface spline for interpolating out-of-plane motion for aeroelastic
problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPLINE1 |  EID  | CAERO | BOX1  | BOX2  | SETG  |   DZ  |       |       |     |
|SPLINE1 |   3   |  111  | 111   | 118   |  14   |   0.  |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (unique Integer > 0).

CAERO      Aero element ID which defines plane of spline (Integer > 0).

BOX1, BOX2 First and last box whose motions are interpolated using this
           spline (Integer > 0).

SETG       Refers to a SETi card which lists the structural grid points to
           which the spline is attached (Integer > 0).

DZ         Linear attachment flexibility (Real >= 0).

## Remarks

1. The interpolated points (k-set) will be defined by aero-cells. The sketch
   shows the cells for which uk is interpolated if BOX1 = 111 and BOX2 = 118.

+----------+----------+----------+----------+
|//////////|//////////|//////////|          |
|///111////|///114////|///117////|   120    |
|//////////|//////////|//////////|          |
+----------+----------+----------+----------+
|//////////|//////////|//////////|          |
|///112////|///115////|///118////|   121    |
|//////////|//////////|//////////|          |
+----------+----------+----------+----------+
|          |          |          |          |
|   113    |   116    |   119    |   122    |
|          |          |          |          |
+----------+----------+----------+----------+

2. The attachment flexibility (units of area) is used for smoothing the
   interpolation. If DZ = 0, the spline will pass through all deflected grid
   points. If DZ >> (area of spline), a least squares plane fit will occur.
   Intermediate values will provide smoothing.


SPLINE2 - Linear Spline
=======================

## Description

Defines a beam spline for interpolating panels and bodies for aeroelastic
problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPLINE2 |  EID  | CAERO |  ID1  |  ID2  | SETG  |   DZ  |  DTOR |  CID  |ABC  |
|+BC     |  DTHX | DTHY  |       |       |       |       |       |       |     |
|SPLINE2 |   5   |   8   |  12   |  24   |  60   |   0.  |  1.0  |   3   |abc  |
|+bc     |  -1.  |       |       |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

CAERO      Aero panel or body which is to be interpolated (Integer > 0).

ID1, ID2   First and last box or body element whose motions are interpolated
           using this spline (Integer > 0).

SETG       Refers to a SETi card which lists the structural g-set to which
           the spline is attached (Integer > 0).

DZ         Linear attachment flexibility (Real >= 0).

DTOR       Torsional flexibility (EI/GJ) (Real > 0; use 1.0 for bodies).

CID        Rectangular coordinate system which defines y-axis of spline
           (Integer >= 0) (not used for bodies, CAERO2).

DTHX, DTHY Rotational attachment flexibility. DTHX is for rotation about the
x-axis; not used for bodies. DTHY is for rotation about the
y-axis; used for slope of bodies (Real).

## Remarks

1. The interpolated points (k-set) will be defined by aero boxes.

2. For panels, the spline axis is the projection of the y-axis of coordinate
   system CID, projected onto the plane of the panel. For bodies, the spline
   axis is parallel to the x-axis of the aerodynamic coordinate system.

3. The flexibilities are used for smoothing. Zero attachment flexibility
   values will imply rigid attachment, that is, no smoothing. (Negative values
   in fields 12 and 13 will imply infinity, hence no attachment.)

4. A continuation card is required.

5. The SPLINE2 EID must be unique with respect to all SPLINEi data cards.


SPLINE3 - Constraint Equation for Aeroelastic Problems
======================================================

## Description

Defines a constraint equation for aeroelastic problems. Useful for control
surface constraints.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPLINE3 |  EID  | CAERO | UKID  | COMP  |  G1   |  C1   |  A1   |       |ABC  |
|+BC     |  G2   |  C2   |  A2   |       |  G3   |  C3   |  A3   |       |     |
|SPLINE3 | 7000  |  107  | 109   |   6   |  33   |   5   |  1.0  |       |abc  |
|+bc     |  43   |  5    | -1.0  |       |       |       |       |       |     |

Field      Contents
-----      --------
EID        Element identification number (Integer > 0).

CAERO      Identification number of macro-element on which the element to be
           interpolated lies (Integer > 0).

UKID       Identification number of the uk point (that is, the box number)
           (Integer > 0).

COMP       The component of motion to be interpolated. 3 = normal rotation, 5
           = pitch angle (for z, yz bodies), 6 = control relative angle (also
           for y-bodies, 2 = lateral displacement and 6 = yaw). (Integer >
           0).

Gi         Grid point identification number of independent grid point
           (Integer > 0).

Ci         Component (in global coordinate system) to be used (one of the
           Integers 1 through 6, or 0 for scalar points).

Ai         Coefficient of constraint relationship (Real).

## Remarks

1. The independent grid points and components must refer to degrees of freedom
   in the ug point set.

2. The constraint is given by

   u  = -Ai u
    d   i    i

   ud = the value of the dependent uk component.
   ui = the displacement at grid Gi, component Ci.

3. The SPLINE3 EID must be unique with respect to all SPLINEi data cards.


SPOINT - Scalar Point
=====================

## Description

Defines scalar points of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPOINT  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |   ID  |     |
|SPOINT  |   3   |   18  |   1   |   4   |   16  |   2   |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SPOINT  |  ID1  |"THRU" |  ID2  |       |       |       |       |       |     |
|SPOINT  |   5   | THRU  |  649  |       |       |       |       |       |     |

Field      Contents
-----      --------
ID, ID1, ID2  Scalar point identification number (Integer > 0; IDl < ID2).

## Remarks

1. Scalar points defined by their appearance on a scalar connection card need
   not appear on an SPOINT card.

2. All scalar point identification numbers must be unique with respect to all
   other structural, scalar, and fluid points.

3. This card is used primarily to define scalar points appearing in single or
   multipoint constraint equations but to which no scalar elements are
   connected.

4. If the alternate form is used, scalar points ID1 through ID2 are defined.

5. For a discussion of scalar points, see Section 5.6 of the Theoretical
   Manual.


STREAML1 - Blade Streamline Grid Data
=====================================

## Description

Defines grid points on a blade streamline from the blade leading edge to the
blade trailing edge.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|STREAML1|  SLN  |   G1  |   G2  |   G3  |   G4  |   G5  |   G6  |   G7  |abc  |
|+bc     |   G8  |   G9  |  etc. |       |       |       |       |       |     |
|STREAML1|   3   |   2   |   4   |   6   |   8   |   10  |   14  |   16  |ABC  |
|+BC     |   20  |   24  |       |       |       |       |       |       |     |

## Alternate Form

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|STREAML1|  SLN  |  GID1 |"THRU" |  GID2 |       |       |       |       |     |
|STREAML1|   5   |   6   | THRU  |   12  |       |       |       |       |     |

Field      Contents
-----      --------
SLN        Streamline number (Integer > 0).

Gi, GIDi   Grid point identification numbers (Integer > 0).

## Remarks

1. This card is required for static aerothermoelastic design/analysis and
   blade cyclic modal flutter problems.

2. There must be one STREAML1 card for each streamline on the blade.

3. For blade cyclic modal flutter problems, there must be an equal number of
   STREAML1 and STREAML2 cards and the streamline number, SLN, must be the
   same on the corresponding cards.

4. The streamline numbers, SLN, must increase with increasing radial distance
   of the blade section from the axis of rotation. The lowest SLN and the
   highest SLN will be assumed to represent the blade sections closest to, and
   farthest from, the axis of rotation, respectively.

5. All grid points should be unique.

6. All grid points referenced by GID1 through G1D2 must exist.

7. All STREAML1 cards must have the same number of grid points. The grid
   points must be input from the blade leading edge to the blade trailing edge
   in the correct positional order.


STREAML2 - Blade Streamline Flow Data
=====================================

## Description

Defines aerodynamic data for a blade streamline.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|STREAML2|  SLN  | NSTNS |STAGGER| CHORD |RADIUS/| BSPACE|  MACH |  DEN  |abc  |
|        |       |       |       |       | DCBDZB|       |       |       |     |
|+bc     |  VEL  | FLOWA/|       |       |       |       |       |       |     |
|        |       |  SWEEP|       |       |       |       |       |       |     |
|STREAML2|   2   |   3   |  23.5 |  1.85 |  6.07 |  .886 |  .934 |  .066 |ABC  |
|+bc     |1014.2 | 55.12 |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SLN        Streamline number (Integer > 0).

NSTNS      Number of computing stations on the blade streamline (Integer, 3
           <= NSTNS <= 10).

STAGGER    Blade stagger angle in degrees (Real, -90.0 < STAGGER < 90.0).

CHORD      Blade chord (Real > 0.0).

RADIUS/DCBDZB  Radius of streamline (for flutter analysis without sweep
           effects) (Real > 0.0) or aC/aZ (for flutter analysis with sweep
           effects) (Real). C is the swept chord and Z is the (local)
           spanwise reference direction. See Remark 4.

BSPACE     Blade spacing (Real > 0.0).

MACH       Relative flow Mach number at blade leading edge (Real > 0.0).

DEN        Gas density at blade leading edge (Real > 0.0).

VEL        Relative flow velocity at blade leading edge (Real > 0.0).

FLOWA/SWEEP  Relative flow angle at blade leading edge (for flutter analysis 
           without sweep effects) or blade sweep angle (for flutter analysis
           with sweep effects) (Real, -90.0 < FLOWA or SWEEP < 90.0 degrees).
           See Remark 4.

## Remarks

1. At least three (3), and no more than fifty (50), STREAML2 cards are
   required for a blade cyclic modal flutter analysis.

2. For blade cyclic modal flutter problems, there must be an equal number of
   STREAML1 and STREAML2 cards and the streamline number, SLN, must be the
   same on the corresponding cards.

3. It is not required that all streamlines be used to define the aerodynamic
   matrices employed in blade flutter analysis.

4. For flutter analysis with sweep effects, the use of the NASTRAN card is
   required as follows (see Sections 1.20 and 2.1):

   NASTRAN SYSTEM(93) = 1


SUPAX - Axisymmetric Fictitious Support
=======================================

## Description

Defines coordinates at which determinate reactions are to be applied during
the analysis of a free body modeled with CCONEAX, CTRAPAX, or CTRIAAX
elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SUPAX   |  RID  |  HID  |   C   |  RID  |  HID  |   C   |       |       |     |
|SUPAX   |       |       |       |   4   |   3   |   2   |       |       |     |

Field      Contents
-----      --------
RID        Ring identification number (Integer > 0).

HID        Harmonic identification number (Integer >= 0).

C          Component number (any unique combination of the digits 1 - 6).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. Up to 12 coordinates may appear on a single card.

3. Coordinates appearing on SUPAX cards may not appear on MPCAX, SPCAX, or
   OMITAX cards.

4. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

5. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


SUPORT - Fictitious Support
===========================

## Description

Defines coordinates at which determinate reactions are to be applied to a free
body during analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|SUPORT  |   ID  |   C   |   ID  |   C   |   ID  |   C   |   ID  |   C   |     |
|SUPORT  |   16  |  215  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
ID         Grid or scalar point identification number (Integer > 0).

C          Component number (zero or blank for scalar points; any unique
           combination of the digits 1 - 6 for grid points).

## Remarks

1. Coordinates defined on this card may not appear on single-point constraint
   cards (SPC, SPC1), on omit cards (OMIT, OMIT1) or as dependent coordinates
   in multipoint constraint equations (MPC) or as degrees of freedom on rigid
   elements (CRIGD1, CRIGD2, CRIGD3, CRIGDR).

2. From one to twenty-four support coordinates may be defined on a single
   card.


TABDMP1 - Structural Damping Table
==================================

## Description

Defines structural damping as a tabular function of frequency.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABDMP1 |   ID  |       |       |       |       |       |       |       |abc  |
|+bc     |  F1   |  G1   |  F2   |  G2   |  F3   |  G3   |  F4   |  G4   |     |
|TABDMP1 |   3   |       |       |       |       |       |       |       |ABC  |
|+BC     |  2.5  |.01057 |  2.6  |.01362 |  ENDT |       |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

Fi         Frequency value in cycles per unit time (Real >= 0.0).

Gi         Damping value (Real).

## Remarks

1. The Fi must be in either ascending or descending order but not both.

2. Jumps (Fi = Fi+1) are allowed, but not at the end points.

3. At least two entries must be present.

4. Any Fi, Gi entry may be ignored by placing the BCD string SKIP in either of
   two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. The TABDMP1 mnemonic implies the use of the algorithm

+--------------+
| G   =  g (F) |
|         T    |
+--------------+

   where F is input to the table and G is returned. The table look-up gT(F) is
   performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average gT(F) is used. There are
   no error returns from this table look-up procedure.

7. Structural damping tables must be selected in the Case Control Deck (SDAMP
   = ID) to be used by NASTRAN.

8. Structural damping is used only in modal formulations of complex eigenvalue
   analysis, frequency response analysis, or transient response analysis.

9. A PARAM, KDAMP, is used in aeroelastic rigid formats to select the type of
   damping. See PARAM bulk data card.


TABLED1 - Dynamic Load Tabular Function
=======================================

## Description

Defines a tabular function for use in generating frequency-dependent and
time-dependent dynamic loads.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLED1 |   ID  |       |       |       |       |       |       |       |+abc |
|+abc    |  X1   |  Y1   |  X2   |  Y2   |  X3   |  Y3   |  X4   |  Y4   |     |
|TABLED1 |   32  |       |       |       |       |       |       |       |ABC  |
|+BC     | -3.0  |  6.9  |  2.0  |  5.6  |  3.0  |  5.6  |  ENDT |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

Xi, Yi     Tabular entries (Real).

## Remarks

1. The Xi must be in either ascending or descending order but not both.

2. Jumps between two points (Xi = Xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any X-Y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEDi mnemonic implies the use of a specific algorithm. For TABLED1
   type tables, this algorithm is

+--------------+
| Y   =  y (X) |
|         T    |
+--------------+

   where X is input to the table and Y is returned. The table look-up yT(x), x
   = X, is performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average yT(x) is used. There are
   no error returns from this table look-up procedure.

7. Linear extrapolation is not used for Fourier Transform methods. The
   function is zero outside the range.


TABLED2 - Dynamic Load Tabular Function
=======================================

## Description

Defines a tabular function for use in generating frequency-dependent and
time-dependent dynamic loads. Also contains parametric data for use with the
table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLED2 |   ID  |  X1   |       |       |       |       |       |       |+abc |
|+abc    |  x1   |  y1   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |+def |
|+def    |  x5   |  y5   |  x6   |  y6   |  x7   |  y7   |  x8   |  y8   |     |
|TABLED2 |   15  | -10.5 |       |       |       |       |       |       |ABC  |
|+BC     |  1.0  | -4.5  |  2.0  | -4.2  |  2.0  |  2.8  |  7.0  |  6.5  |DEF  |
|+EF     | SKIP  | SKIP  |  9.0  |  6.5  |  ENDT |       |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1         Table parameter (Real).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. Jumps between two points (xi = xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any X-Y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEDi mnemonic implies the use of a specific algorithm. For TABLED2
   type tables, this algorithm is

+------------------+
| Y   =  y (X -X1) |
|         T        |
+------------------+

   where X is input to the table and Y is returned. The table look-up yT(x), x
   = X-X1, is performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average yT(x) is used. There are
   no error returns from this table look-up procedure.

7. Linear extrapolation is not used for Fourier Transform methods. The
   function is zero outside the range.


TABLED3 - Dynamic Load Tabular Function
=======================================

## Description

Defines a tabular function for use in generating frequency-dependent and
time-dependent dynamic loads. Also contains parametric data for use with the
table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLED3 |   ID  |   X1  |   X2  |       |       |       |       |       |+abc |
|+abc    |  x1   |  Yy   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |     |
|TABLED3 |   62  | 126.9 |  30.0 |       |       |       |       |       |ABC  |
|+BC     |  2.9  |  2.9  |  3.6  |  4.7  |  5.2  |  5.7  |  ENDT |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1, X2     Table parameters (Real; X2 not equal 0.0).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. Jumps between two points (xi = xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any X-Y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEDi mnemonic implies the use of a specific algorithm. For TABLED3
   type tables, this algorithm is

+-------------------+
|          +(X -X1)+|
| Y   =  y |-------||
|         T+   X2  +|
+-------------------+

   where X is input to the table and Y is returned. The table look-up yT(x), x
   = (X-X1)/X2, is performed using linear interpolation within the table and
   linear extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average yT(x) is used. There are
   no error returns from this table look-up procedure.

7. Linear extrapolation is not used for Fourier Transform methods. The
   function is zero outside the range.


TABLED4 - Dynamic Load Tabular Function
=======================================

## Description

Defines coefficients of a power series for use in generating
frequency-dependent and time-dependent dynamic loads. Also contains parametric
data for use with the table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLED4 |   ID  |   X1  |   X2  |   X3  |   X4  |       |       |       |+abc |
|+abc    |  A0   |  A1   |  A2   |  A3   |  A4   |  A5   |  A6   |  A7   |+def |
|TABLED4 |   28  |  0.0  |  1.0  |  0.0  |  100. |       |       |       |ABC  |
|+BC     |  2.91 |-0.0329| 6.51-5|  0.0  |-3.4-7 |  ENDT |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1,...,X4  Table parameters (Real; X2 not equal 0.0; X3 < X4).

Ai         Coefficient entries (Real).

## Remarks

1. At least one entry must be present.

2. The end of the table is indicated by the existence of the BCD string ENDT
   in the field following the last entry. An error is detected if any
   continuation cards follow the card containing the end-of-table flag ENDT.

3. Each TABLEDi mnemonic implies the use of a specific algorithm. For TABLED4
   type tables, this algorithm is

+------------------------+
|        N     +(X -X1)+i|
| Y   =  -  A  |-------| |
|       i=0  i +   X2  + |
+------------------------+

   where X is input to the table and Y is returned. Whenever X < X3, use X3
   for X; whenever X > X4, use X4 for X. There are N + 1 entries in the table.
   There are no error returns from this table look-up procedure.


TABLEM1 - Material Property Table
=================================

## Description

Defines a tabular function for use in generating temperature dependent
material properties.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLEM1 |   ID  |       |       |       |       |       |       |       |+abc |
|+abc    |  x1   |  y1   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |+def |
|TABLEM1 |   32  |       |       |       |       |       |       |       |ABC  |
|+BC     | -3.0  |  6.9  |  2.0  |  5.6  |  3.0  |  5.6  |  ENDT |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. Jumps between two points (xi = xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any x-y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEMi mnemonic implies the use of a specific algorithm. For TABLEM1
   type tables, this algorithm is

+--------------+
| Y   =  y (X) |
|         T    |
+--------------+

   where X is input to the table and Y is returned. The table look-up yT(x), x
   = X, is performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average yT(x) is used. There are
   no error returns from this table look-up procedure.


TABLEM2 - Material Property Table
=================================

## Description

Defines a tabular function for use in generating temperature dependent
material properties. Also contains parametric data for use with the table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLEM2 |   ID  |  X1   |       |       |       |       |       |       |+abc |
|+abc    |  x1   |  y1   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |+def |
|+def    |  x5   |  y5   |  x6   |  y6   |  x7   |  y7   |  x8   |  y8   |     |
|TABLEM2 |   15  | -10.5 |       |       |       |       |       |       |ABC  |
|+BC     |  1.0  | -4.5  |  2.0  | -4.5  |  2.0  |  2.8  |  7.0  |  6.5  |DEF  |
|+EF     | SKIP  | SKIP  |  9.0  |  6.5  |  ENDT |       |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1         Table parameter (Real).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. Jumps between two points (xi = xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any x-y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEMi mnemonic implies the use of a specific algorithm. For TABLEM2
   type tables, this algorithm is

+------------------+
| Y  = Z y (X -X1) |
|         T        |
+------------------+

   where X is input to the table, Y is returned, and Z is supplied from the
   basic MATi card. The table look-up yT(x), x = X - X1, is performed using
   linear interpolation within the table and linear extrapolation outside the
   table using the last two end points at the appropriate table end. At jump
   points the average yT(x) is used. There are no error returns from this
   table look-up procedure.

# TABLEM3 - Material Property Table

## Description

Defines a tabular function for use in generating temperature dependent
material properties. Also contains parametric data for use with the table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLEM3 |  ID   |  X1   |  X2   |       |       |       |       |       |+abc |
|+abc    |  x1   |  y1   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |+def |
|TABLEM3 |  62   | 126.9 | 30.0  |       |       |       |       |       |+ABC |
|+BC     | 2.9   | 2.9   | 3.6   | 4.7   | 5.2   | 5.7   | ENDT  |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1, X2     Table parameters (Real; X2 not equal 0.0).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. Jumps between two points (xi = xi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any x-y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. Each TABLEMi mnemonic implies the use of a specific algorithm. For TABLEM3
   type tables, this algorithm is

+-------------------+
| Y  = Z y  (X -X1) |
|         T --------|
|              X2   |
+-------------------+

   where X is input to the table, Y is returned, and Z is supplied from the
   basic MATi card. The table look-up yT(x), x = (X - X1)/X2, is performed
   using linear interpolation within the table and linear extrapolation
   outside the table using the last two end points at the appropriate table
   end. At jump points the average yT(x) is used. There are no error returns
   from this table look-up procedure.


# TABLEM4 - Material Property Table

## Description

Defines coefficients of a power series for use in generating temperature
dependent material properties. Also contains parametric data for use with the
table.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLEM4 |  ID   |  X1   |  X2   |  X3   |  X4   |       |       |       |+abc |
|+abc    |  A0   |  A1   |  A2   |  A3   |  A4   |  A5   |  A6   |  A7   |+def |
|TABLEM4 |  28   | 0.0   | 1.0   | 0.0   | 100.  |       |       |       |ABC  |
|+BC     | 2.91  |-0.0329|6.51-5 | 0.0   |-3.4-7 | ENDT  |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

X1,...,X4  Table parameters (Real; X2 not equal 0.0; X3 < X4).

Ai         Coefficient entries (Real).

## Remarks

1. At least one entry must be present.

2. The end of the table is indicated by the existence of the BCD string ENDT
   in the field following the last entry. An error is detected if any
   continuation cards follow the card containing the end-of-table flag ENDT.

3. Each TABLEMi mnemonic implies the use of a specific algorithm. For TABLEM4
   type tables, this algorithm is

+------------------------+
|        N     +(X -X1)+i|
| Y = Z  -  A  |-------| |
|       i=0  i +   X2  + |
+------------------------+

   where X is input to the table, Y is returned, and Z is supplied from the
   basic MATi card. Whenever X < X3, use X3 for X; whenever X > X4, use X4 for
   X. There are N + 1 entries in the table. There are no error returns from
   this table look-up procedure.


# TABLES1 - Tabular Stress-Strain Function

## Description

Defines a tabular stress-strain function for use in piecewise linear analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABLES1 |  ID   |       |       |       |       |       |       |       |+abc |
|+abc    |  x1   |  y1   |  x2   |  y2   |  x3   |  y3   |  x4   |  y4   |+def |
|TABLES1 |  32   |       |       |       |       |       |       |       |ABC  |
|+BC     | -3.0  |  6.9  |  2.0  |  5.6  |  3.0  |  5.6  | ENDT  |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

xi, yi     Tabular entries (Real).

## Remarks

1. The xi must be in either ascending or descending order but not both.

2. For piecewise linear analysis, the yi numbers must form a non-decreasing
   sequence for an ascending xi sequence and vice versa.

3. Jumps between two points (xi = xi+l) are allowed, but not at the end
   points.

4. At least two entries must be present.

5. Any x-y entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

6. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

7. Each TABLESi mnemonic implies the use of a specific algorithm. For TABLES1
   type tables, this algorithm is

+--------------+
| Y   =  y (X) |
|         T    |
+--------------+

   where X is input to the table and Y is returned. The table look-up yT(x), x
   = X, is performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average yT(x) is used. There are
   no error returns from this table look-up procedure.

8. The table may have a zero slope only at its end.

# TABRND1 - Power Spectral Density Table

## Description

Defines power spectral density as a tabular function of frequency for use in
random analysis. Referenced on the RANDPS card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABRND1 |  ID   |       |       |       |       |       |       |       |abc  |
|+bc     |  f1   |  g1   |  f2   |  g2   |  f3   |  g3   |  f4   |  g4   |def  |
|TABRND1 |  3    |       |       |       |       |       |       |       |ABC  |
|+BC     | 2.5   |.01057 | 2.6   |.01362 | ENDT  |       |       |       |     |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

fi         Frequency value in cycles per unit time (Real not equal 0.0).

gi         Power spectral density (Real).

## Remarks

1. The fi must be in either ascending or descending order but not both.

2. Jumps between two points (fi = fi+1) are allowed, but not at the end
   points.

3. At least two entries must be present.

4. Any f-g entry may be ignored by placing the BCD string SKIP in either of
   the two fields used for that entry.

5. The end of the table is indicated by the existence of the BCD string ENDT
   in either of the two fields following the last entry. An error is detected
   if any continuation cards follow the card containing the end-of-table flag
   ENDT.

6. The TABRND1 mnemonic implies the use of the algorithm

+--------------+
| G   =  g (F) |
|         T    |
+--------------+

   where F is input to the table and G is returned. The table look-up gT(F) is
   performed using linear interpolation within the table and linear
   extrapolation outside the table using the last two end points at the
   appropriate table end. At jump points the average gT(F) is used. There are
   no error returns from this table look-up procedure.

# TABRNDG - Gust Power Spectral Density

## Description

Defines the power spectral density of a gust for aeroelastic analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TABRNDG |  ID   | TYPE  |  LU   |  WG   |       |       |       |       |     |
|TABRNDG |  3    |   1   | 1.3   |  1.   |       |       |       |       |ABC  |

Field      Contents
-----      --------
ID         Table identification number (Integer > 0).

TYPE       Choice of Von Karman (TYPE = 1) or Dryden model (TYPE = 2)
           (Integer 1 or 2).

LU         L/U, scale of turbulence divided by velocity (units of time)
           (Real).

WG         Root-mean-square gust velocity.

## Remarks

1. This card must be referenced on a RANDPS data card.

2. The power spectral density is given by:

                            1+2(p+l)k**2(L/U)**2w**2
   Sq(w) = 2(WG)**2(L/U) -----------------------------
                         [1+k**2(L/U)**2w**2]**(p+3/2)

   where

+--------------+------+--------+
|     Type     |  p   |   k    |
+--------------+------+--------+
| 1=Von Karman | l/3  | l.339  |
| 2=Dryden     | l/2  | l.0    |
+--------------+------+--------+

   and w = 2*pi*f. The units of Sq(w) are velocity squared per Hertz.

3. Other PSD functions may be defined using the TABRND1 data card.

# TEMP - Grid Point Temperature Field

## Description

Defines temperature at grid points for determination of:

   1. Thermal loading
   2. Temperature-dependent material properties
   3. Stress recovery

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMP    |  SID  |   G   |   T   |   G   |   T   |   G   |   T   |       |     |
|TEMP    |   3   |  94   | 316.2 |  49   | 219.8 |       |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

G          Grid point identification number (Integer > 0).

T          Temperature (Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. From one to three grid point temperatures may be defined on a single card.

3. If thermal effects are requested, all elements must have a temperature
   field defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card
   or indirectly as the average of the connected grid point temperatures
   defined on the TEMP or TEMPD cards. Directly defined element temperatures
   always take precedence over the average of grid point temperatures.

4. If the element material is temperature dependent, its properties are
   evaluated at the average temperature. In the case of isoparametric
   hexahedron elements, their properties are evaluated at the temperature
   computed by interpolating the grid point temperatures.

5. Average element temperatures are obtained as a simple average of the
   connecting grid point temperatures when no element temperature data are
   defined.

6. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

7. In heat transfer analysis, the TEMP card is used for the following special
   purposes:

   a. The Case Control card, TEMP(MATERIAL), will select the initial estimated
      temperature field for nonlinear conductivity and radiation effects. See
      Section 1.8.
   b. Boundary temperatures are defined in Rigid Format 3, HEAT by the Case
      Control card, TEMP(MATERIAL). These points are specified with SPC cards.
   c. The Case Control card, IC, will select the initial conditions, that is,
      grid point temperatures, in transient analysis.


# TEMPAX - Axisymmetric Temperature

## Description

Defines temperature sets for a model containing CCONEAX, CTRAPAX, or CTRIAAX
elements.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPAX  |  SID  |  RID  |  PHI  | TEMP  |  SID  |  RID  |  PHI  | TEMP  |     |
|TEMPAX  |   4   |   7   | 30.0  |105.3  |       |       |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

RID        Ring identification number (see RINGAX card) (Integer > 0).

PHI        Azimuthal angle in degrees (Real).

TEMP       Temperature (Real).

## Remarks

1. This card is allowed if and only if an AXIC card is also present.

2. One or two temperatures may be defined on each card.

3. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

4. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

5. At least two different angles are required for each RID and temperature set
   to specify the subtended angle [íb-ía] over which the temperature applies.

6. For a discussion of the conical shell problem, see Section 5.9 of the
   Theoretical Manual.

7. For a discussion of the axisymmetric solid problem, see Section 5.11 of the
   Theoretical Manual.


# TEMPD - Grid Point Temperature Field Default

## Description

Defines a temperature default for all grid points of the structural model
which have not been given a temperature on a TEMP card.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPD   | SID   |   T   | SID   |   T   | SID   |   T   | SID   |   T   |     |
|TEMPD   |  1    |216.3  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

T          Default temperature (Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. From one to four default temperatures may be defined on a single card.

3. If thermal effects are requested, all elements must have a temperature
   field defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card
   or indirectly as the average of the connected grid point temperatures
   defined on the TEMP or TEMPD cards. Directly defined element temperatures
   always take precedence over the average of grid point temperatures.

4. If the element material is temperature dependent its properties are
   evaluated at the average temperature. In the case of isoparametric
   hexahedron elements, their properties are evaluated at the temperature
   computed by interpolating the grid point temperatures.

5. Average element temperatures are obtained as a simple average of the
   connecting grid point temperatures when no element temperature data are
   defined.

6. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

7. In heat transfer analysis, the TEMP card is used for the following special
   purposes:

   a. The Case Control card, TEMP(MATERIAL), will select the initial estimated
      temperature field for nonlinear conductivity and radiation effects. See
      Section 1.8.
   b. Boundary temperatures are defined in Rigid Format 3, HEAT, by the Case
      Control card, TEMP(MATERIAL). These points are specified with SPC cards.
   c. The Case Control card, IC, will select the initial conditions, that is,
      grid point temperatures, in transient analysis.


# TEMPP1 - Plate Element Temperature Field

## Description

Defines a temperature field for plate, membrane, and combination elements (by
an average temperature and a thermal gradient over the cross-section) for
determination of:

   1. Thermal loading
   2. Temperature-dependent material properties
   3. Stress recovery

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPP1  |  SID  | EID1  |   T   |   T'  |   T1  |   T2  |       |       |+abc |
|+abc    | EID2  | EID3  | EID4  | EID5  | EID6  | EID7  | EID8  | EID9  |+def |
|TEMPP1  |   2   |  24   |  62.0 |  10.0 |  57.0 |  67.0 |       |       |A1A  |
|+1A     |  26   |  21   |  19   |  30   |       |       |       |       |     |

## Alternate Form of Continuation Card

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+abc    | EID2  |"THRU" | EIDi  | EIDj  |"THRU" | EIDk  |       |       |+def |
|+1A     |  1    | THRU  |  10   |  30   | THRU  |  61   |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

EIDn       Unique element identification number(s) (Integer > 0 or BCD: the
           continuation card may have THRU in fields 3 and/or 6, in which
           case EID2 < EIDi, EIDj < EIDk).

T          Average temperature over the cross-section. Assumed constant over
           area (Real).

T'         Effective linear thermal gradient. Not used for membranes (Real).

T1, T2     Temperatures for stress calculation, at points defined on the
           element property card. Z1 and Z2 are given on PTRBSC, PQDPLT,
           PTRPLT, PTRIA1, and PQUAD1 cards. T1 may be specified on the lower
           surface and T2 on the upper surface for the QUAD2 and TRIA2
           elements. These data are not used for membrane elements (Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. If continuation cards are present, EID1 and elements specified on the
   continuation card(s) are used. Elements must not be specified more than
   once.

3. If thermal effects are requested, all elements must have a temperature
   field defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card
   or indirectly as the average of the connected grid point temperatures
   defined on the TEMP or TEMPD cards. Directly defined element temperatures
   always take precedence over the average of grid point temperatures.

4. For a temperature field other than a constant gradient the effective
   gradient for a homogeneous plate is:

           1   ô
   T'  =  ---  õ T(z)z dz
           I   z

   where I is the bending inertia, and z is the distance from the neutral
   surface in the positive normal direction.

5. The average temperature for a homogeneous plate is

             1      ô
   T  =   --------  õ  T dVolume
           Volume   Volume

6. If the element material is temperature dependent, its properties are
   evaluated at the average temperature T.

7. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

# TEMPP2 - Plate Element Temperature Field

## Description

Defines a temperature field for plate, membrane, and combination elements by
an average temperature and thermal moments for determination of:

   1. Thermal loading
   2. Temperature-dependent material properties
   3. Stress recovery

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPP2  |  SID  | EID1  |   T   |  MX   |  MY   | MXY   |  T1   |  T2   |+abc |
|+abc    | EID2  | EID3  | EID4  | EID5  | EID6  | EID7  | EID8  | EID9  |+def |
|TEMPP2  |   2   |  36   | 68.8  |       |       |       |       |       |XYZ  |
|+YZ     | 400   |   1   |   2   |   5   |       |       |       |       |     |

## Alternate Form of Continuation Card

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+abc    | EID2  |"THRU" | EIDi  | EIDj  |"THRU" | EIDk  |       |       |+def |
|+YZ     |  37   | THRU  | 312   | 315   | THRU  | 320   |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

EIDn       Unique element identification number(s) (Integer > 0 or BCD: a
           continuation card may have THRU in field 3 and/or 6 in which case
           EID2 < EIDi, EIDj < EIDk).

T          Average temperature over cross-section. Assumed constant over area
           (Real).

MX, MY, MXY  Resultant thermal moments per unit width in element coordinate 
           system. Not used for membrane elements (Real).

T1, T2     Temperature for stress calculation at points defined on the
           element property card. Z1 and Z2 are given on PTRBSC, PQDPLT,
           PTRPLT, PTRIA1, and PQUAD1 cards. T1 may be specified on the lower
           surface and T2 on the upper surface for the QUAD2 and TRIA2
           elements. These data are not used for membrane elements (Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. If continuation cards are present, EID1 and elements specified on the
   continuation card(s) are used. Elements must not be specified more than
   once.

3. If thermal effects are requested all elements must have a temperature field
   defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card or
   indirectly as the average of the connected grid point temperatures defined
   on the TEMP or TEMPD cards. Directly defined element temperatures always
   take precedence over the average of grid point temperatures.

4. The thermal moments in the element coordinate system may be calculated from
   the formula:

   Mx       ô
   My   = - | [Ge] {+ } T(z)z dz
   Mxy      õ        e

   where the integration is performed over the bending material properties in
   the element coordinate system.

   [Ge] 3x3 elastic coefficient matrix

   {+e} 3x1 material thermal expansion coefficients

   T(z) temperature at z

   z    distance from the neutral surface in the element coordinate system.

5. The temperature dependent material properties are evaluated at the average
   temperature T. If a property varies with depth, an effective value must be
   used which satisfies the desired elastic and stress relationships. The
   temperatures at the fiber distances may be changed to compensate for local
   differences in +e and produce correct stresses.

6. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.


# TEMPP3 - Plate Element Temperature Field

## Description

Defines a temperature field for homogeneous plate, membrane, and combination
elements (by a tabular description of the thermal field over the
cross-section) for determination of:

   1. Thermal loading
   2. Temperature-dependent material properties
   3. Stress recovery.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPP3  | SID   | EID1  |  Z0   |  T0   |  Z1   |  T1   |  Z2   |  T2   |+abc |
|+abc    |  Z3   |  T3   |  Z4   |  T4   |  Z5   |  T5   |  Z6   |  T6   |+def |
|+def    |  Z7   |  T7   |  Z8   |  T8   |  Z9   |  T9   | Z10   | T10   |+ghi |
|+ghi    | EID2  | EID3  | EID4  | EID5  | EID6  | EID7  | EID8  | EID9  |+jkl |
|TEMPP3  |  17   |  39   | 0.0   | 32.9  | 2.0   | 43.4  | 2.5   | 45.0  |XY1  |
|+Y1     | 3.0   | 60.0  | 4.0   | 90.0  |       |       |       |       |XY2  |
|+Y2     |       |       |       |       |       |       |       |       |XY3  |
|+Y3     |   1   |   2   |   3   |   4   |   5   |   6   |   8   |  10   |     |

## Alternate Form of Continuation Card Number 3

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+ghi    | EID2  |"THRU" | EIDi  | EIDj  |"THRU" | EIDk  |       |       |+jkl |
|+Y3     |       |       |       |  1    | THRU  |  10   |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer > 0).

EIDn       Unique element identification number(s) (Integer > 0 or BCD: the
           continuation card may have THRU in fields 3 and/or 6 in which case
           EID2 < EIDi, EIDj < EIDk).

Z0         Position of the bottom surface with respect to an arbitrary
           reference plane (Real).

Zi         Positions on cross-section from bottom to top of cross-section
           relative to the arbitrary reference plane. There must be an
           increasing sequence with the last nonzero value corresponding to
           the top surface (Real).

T0         Temperature at the bottom surface (Real).

Ti         Temperature at position Zi (Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. If the third (and succeeding) continuation card is present, EID1 and
   elements specified on the third (and succeeding) continuation cards are
   used. Elements must not be specified more than once.

3. The first and second continuation card must be present if a list of
   elements is to be used.

4. If thermal effects are requested, all elements must have a temperature
   field defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card
   or indirectly as the average of the connected grid point temperatures
   defined on the TEMP or TEMPD cards. Directly defined element temperatures
   always take precedence over the average of grid point temperatures.

5. If the element material is temperature dependent, its properties are
   evaluated at the average temperature over the depth which is calculated by
   the program using a linear distribution between points.

6. For stress recovery, the temperatures at the extreme points z0 and zN are
   assigned to the bottom surface and the top surface of the elements
   specified on either PTRIA2 or QUAD2 data card.

7. The data is limited to a maximum of eleven points on the temperature-depth
   profile.

8. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

# TEMPRB - One-Dimensional Element Temperature Field

## Description

Defines a temperature field for the BAR, ROD, TUBE, and CONROD elements for
determination of:

   1. Thermal loading
   2. Temperature-dependent material properties
   3. Stress recovery

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TEMPRB  |  SID  | EID1  |  TA   |  TB   | T'1a  | T'1b  | T'2a  | T'2b  |+abc |
|+abc    |  TCa  |  TDa  | TEa   | TFa   | TCb   | TDb   | TEb   | TFb   |+def |
|+def    | EID2  | EID3  | EID4  | EID5  | EID6  | EID7  | EID8  | EID9  |+ghi |
|TEMPRB  |  200  |  1    | 68.0  | 23.0  | 0.0   | 28.0  |       | 2.5   |AXY10|
|+XY10   | 68.0  | 91.0  | 45.0  |       | 48.0  | 80.0  | 20.0  |       |AXY20|
|+XY20   |  9    |  10   |       |       |       |       |       |       |     |

## Alternate Form for Continuation Card Number 2

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|+def    | EID2  |"THRU" | EIDi  | EIDj  |"THRU" | EIDk  |       |       |+ghi |
|+XY20   |  2    | THRU  |  4    |  10   | THRU  |  14   |       |       |     |

Field      Contents
-----      --------
SID        Temperature set identification number (Integer 0).

EIDn       Unique element identification number(s) (Integer > 0 or BCD: the
           second continuation card may have THRU in fields 3 and/or 6 in
           which case EID2 < EID1, EIDj < EIDk).

TA, TB     Average temperature over the area at end a and end b (Real).

T'ij       Effective linear gradient in direction i on end j (BAR only,
           Real).

Tij        Temperatures at point i as defined on the PBAR card(s) at end j.
           These data are used for stress recovery only (BAR only, Real).

## Remarks

1. Temperature sets must be selected in the Case Control Deck (TEMP = SID) to
   be used by NASTRAN.

2. If at least one nonzero or nonblank Tij is present, the point temperatures
   given are used for stress recovery. If no Tij values are given, linear
   temperature gradients are assumed for stresses.

3. If the second (and succeeding) continuation card is present, EID1 and
   elements specified on the second (and succeeding) continuation cards are
   used. Elements must not be specified more than once.

4. If thermal effects are requested, all elements must have a temperature
   field defined either directly on a TEMPP1, TEMPP2, TEMPP3, or TEMPRB card
   or indirectly as the average of the connected grid point temperatures
   defined on the TEMP or TEMPD cards. Directly defined element temperatures
   always take precedence over the average of grid point temperatures.

5. The effective thermal gradients in the element coordinate system for the
   BAR element are defined by the following integrals over the cross-section.
   For end a (end b is similar):

             1   ô
   T'    =  ---  õ T (y,z)y dA
    1a       I   A  a
              1

             1   ô
   T'    =  ---  õ T (y,z)z dA
    2a       I   A  a
              2

   where Ta(y,z) is the temperature at point y,z (in the element coordinate
   system) at end a of the BAR. See Section 1.3, Figure 1.3-1 for the element
   coordinate system: I1 and I2 are the moment of inertia about the z and y
   axis respectively. The temperatures are assumed to vary linearly along the
   length (x-axis). Note that if the temperature varies linearly over the
   cross-section then T'1a, T'1b, T'2a, and T'2b are the actual gradients.

6. If the element material is temperature dependent, the material properties
   are evaluated at the average temperature

    T  + T
     A    B
   -----------
        2

7. Set ID must be unique with respect to all other LOAD type cards if
   TEMP(LOAD) is specified in the Case Control Deck.

# TF - Dynamic Transfer Function

## Description

1. May be used to define a transfer function of the form

                  2                                2
   (BO + B1p + B2p )u  + - (AO(i) + A1(i)p + A2(i)p )u  = 0
                     d   i                            i

2. May be used as a means of direct matrix input.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TF      | SID   |  GD   |  CD   |  B0   |  B1   |  B2   |       |       |+abc |
|+abc    | G(1)  | C(1)  |A0(1)  |A1(1)  |A2(1)  |       |       |       |+def |
|TF      |  1    |   2   |   3   |  4.0  |  5.0  |  6.0  |       |       |     |
|+ABC    |  3    |   4   | 5.0   | 6.0   | 7.0   |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

GD, G(i)   Grid, scalar, or extra point identification numbers (Integer > 0).

CD, C(i)   Component numbers (null or zero for scalar or extra points, any
           one of the digits 1 - 6 for a grid point).

B0, B1, B2; A0(i), A1(i), A2(i)  Transfer function coefficients (Real).

## Remarks

1. The matrix elements defined by this card are added to the dynamic matrices
   for the problem.

2. Transfer function sets must be selected in the Case Control Deck (TFL =
   SID) to be used by NASTRAN.

3. The constraint relation given above will hold only if no elements are
   connected to the dependent coordinate.


# TIC - Transient Initial Condition

## Description

Defines values for the initial conditions of coordinates used in transient
analysis. Both displacement and velocity values may be specified at
independent coordinates of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TIC     |  SID  |   G   |   C   |  U0   |  V0   |       |       |       |     |
|TIC     |   1   |   3   |   2   |  5.0  | -6.0  |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

G          Grid or scalar or extra point identification number (Integer > 0).

C          Component number (blank or zero for scalar or extra points, any
           one of the digits 1 - 6 for a grid point).

U0         Initial displacement value (Real).

V0         Initial velocity value (Real).

## Remarks

1. Transient initial condition sets must be selected in the Case Control Deck
   (IC = SID) to be used by NASTRAN for structural analysis; however, this
   card should not be used to define initial temperatures in heat transfer
   analysis. (See Section 2.3.)

2. If no TIC set is selected in the Case Control Deck, all initial conditions
   are assumed zero.

3. Initial conditions for coordinates not specified on TIC cards will be
   assumed zero.

4. Initial conditions may be used only in direct formulation.


# TICS - Transient Initial Condition, Substructure Analysis

## Description

Defines values for the initial conditions of coordinates used in direct
transient analysis. Both displacement and velocity values may be specified at
independent coordinates of the structural model.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TICS    |  SID  | NAME  |   G   |   C   |  U0   |  V0   |       |       |     |
|TICS    |   1   | SPAR  |   3   |   2   | 5.0   | -6.0  |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

NAME       Basic substructure name.

G          Grid or scalar or extra point identification number (Integer > 0).

C          Component number (null or zero for scalar or extra points, any one
           of the digits 1 - 6 for a grid point).

U0         Initial displacement value (Real).

V0         Initial velocity value (Real).

## Remarks

1. Transient initial condition sets must be selected in the Case Control Deck
   (IC = SID) to be used by NASTRAN.

2. If no TIC set is selected in the Case Control Deck, all initial conditions
   are assumed zero.

3. Initial conditions for coordinates not specified on TIC cards will be
   assumed zero.

4. Initial conditions may be used only in direct formulation (Rigid Format 9)
   and may only be applied to the analysis of degrees of freedom, that is,
   only those coordinates retained in the solution substructure and not
   constrained using MPC, SPC, or OMIT data.

5. Used in substructure SOLVE operation.


# TLOAD1 - Transient Response Dynamic Load

## Description

Defines a time-dependent dynamic load of the form

   {P(t)} = {A F(t - ç)}

for use in transient response problems.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TLOAD1  |  SID  |   L   |   M   |       |  TF   |       |       |       |     |
|TLOAD1  |   5   |   7   |   9   |       |  13   |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

L          Identification number of DAREA card set or a thermal load set
           which defines A (Integer > 0). For automated multi-stage
           substructuring, reference a DAREAS card set. If desired, the set
           identification may also reference LOADC cards.

M          Identification number of DELAY or DELAYS card set which defines ç
           (Integer >= 0).

TF         Identification number of TABLEDi card which gives F(t - ç)
           (Integer > 0).

## Remarks

1. If M is zero, ç will be zero.

2. Field 5 must be blank.

3. Dynamic load sets must be selected in the Case Control Deck (DLOAD = SID)
   to be used by NASTRAN.

4. TLOAD1 loads may be combined with TLOAD2 loads only by specification on a
   DLOAD card. That is, the SID on a TLOAD1 card may not be the same as that
   on a TLOAD2 card.

5. SID must be unique for all TLOAD1, TLOAD2, RLOAD1, and RLOAD2 cards.

6. Field 3 may reference sets containing QHBDY, QBDY1, QBDY2, QVECT, and QVOL
   cards when using the heat transfer option.

7. If the heat transfer option is used, the referenced QVECT data card may
   also contain references to functions of time, and therefore A may be a
   function of time.

8. Fourier analysis will be used if this is selected in an aeroelastic
   response problem.

9. With automated multi-stage substructuring, DAREAS cards may only reference
   degrees of freedom in the boundary set of the solution structure.

10.   When L references LOADC cards, DAREAS cards with the same set
      identification and non-zero loads must also exist.


# TLOAD2 - Transient Response Dynamic Load

## Description

Defines a time-dependent dynamic load of the form

              É
              º      ~         ~
              º {O}, t < 0  or t > T2 - T1
              º
   {P(t)}  =  º É                        »
              º º       ~                º
              º º  ~B  Ct          ~     º        ~
              º ºA t  e   cos (2piFt + P)º , 0 <= t <= T2 - T1
              º È                        ¼
              È

for use in transient response problems where t-tilde = t - T1 - ç.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TLOAD2  |  SID  |   L   |   M   |       |  T1   |  T2   |   F   |   P   |+abc |
|+bc     |   C   |   B   |       |       |       |       |       |       |     |
|TLOAD2  |   4   |  10   |   7   |       |  2.1  |  4.7  | 12.0  | 30.0  |+12  |
|+12     |  2.0  |  3.0  |       |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

L          Identification number of DAREA card set or a thermal load set
           which defines A (Integer > 0). For automated multi-stage
           substructuring, reference a DAREAS card set. If desired, the set
           identification may also reference LOADC cards.

M          Identification number of DELAY or DELAYS card set which defines ç
           (Integer >= 0).

T1         Time constant (Real >= 0.0).

T2         Time constant (Real, T2 > T1).

F          Frequency in cycles per unit time (Real >= 0.0).

P          Phase angle in degrees (Real).

C          Exponential coefficient (Real).

B          Growth coefficient (Real).

## Remarks

1. If M is zero, ç will be zero.

2. Field 5 must be blank.

3. Dynamic load sets must be selected in the Case Control Deck (DLOAD = SID)
   to be used by NASTRAN.

4. TLOAD2 loads may be combined with TLOAD1 loads only by specification on a
   DLOAD card. That is, the SID on a TLOAD2 card may not be the same as that
   on a TLOAD1 card.

5. SID must be unique for all TLOAD1, TLOAD2, RLOAD1, and RLOAD2 cards.

6. Field 3 may reference load sets containing QHBDY, QBDY1, QBDY2, QVECT,
   QVOL, and SLOAD cards when using the heat transfer option.

7. If the heat transfer option is being used, the referenced QVECT load card
   may also contain references to functions of time, and therefore A may be a
   function of time.

8. Fourier analysis will be used if this selection is an aeroelastic response
   problem.


# TRANS - Component Substructure Transformation Definition

## Description

Defines the location and orientation of the component substructure basic
coordinate system axes relative to the basic coordinate system of the
substructure formed as a result of the substructure COMBINE operation. The
translation and rotation matrices are defined by specifying the coordinates of
three points: A, B, C. The coordinates of points A, B, C must be expressed on
this card in the basic coordinate system of the resultant combined
substructure as follows:

   A  defines the location of the origin of the basic coordinate system of the
      component substructure.

   B  defines the location of a point on the z axis of the basic coordinate
      system of the component substructure.

   C  defines the location of a point in the positive x side of the xz plane
      of the basic coordinate system of the component substructure.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TRANS   |  SID  |       |  A1   |  A2   |  A3   |  B1   |  B2   |  B3   |+abc |
|+bc     |  C1   |  C2   |  C3   |       |       |       |       |       |     |
|TRANS   |   1   |       |  0.0  |  0.0  |  0.0  |  0.0  | -0.5  | 10.0  |ABC  |
|+BC     |  0.0  | 10.0  |  0.5  |       |       |       |       |       |     |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

A1, A2, A3; C1, C2, C3; B1, B2, B3  Coordinates of the points defining system
           as described above.

## Remarks

1. Continuation card must be present.

2. Coordinates A, B, C are given in BASIC coordinate system of the result
   substructure.

3. The value of SID must be unique with respect to all other TRANS data cards.

4. Transformation sets for a whole substructure must be selected in the
   substructure Control Deck (TRANS = SID) to be used by NASTRAN. Note that
   TRANS is a subcommand of the substructure COMBINE command.

5. Transformation of individual grid points in a substructure prior to
   combining them is requested by the GTRAN Bulk Data card which references
   the TRANS information.

6. The three points (A1, A2, A3), (B1, B2, B3), (C1, C2, C3) must be unique
   and non-collinear.


# TSTEP - Transient Time Step

## Description

Defines time step intervals at which solution will be generated and output in
transient analysis.

## Format and Example

|   1    |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   | 10  |
|--------|-------|-------|-------|-------|-------|-------|-------|-------|-----|
|TSTEP   |  SID  |  N(1) | DT(1) | NO(1) |       |       |       |       |+abc |
|+abc    |       |  N(2) | DT(2) | NO(2) |       |       |       |       |+def |
|TSTEP   |   2   |  10   | .001  |  5    |       |       |       |       |+ABC |
|+ABC    |       |   9   | 0.01  |  1    |       |       |       |       |+DEF |

Field      Contents
-----      --------
SID        Set identification number (Integer > 0).

N(i)       Number of time steps of value DT(i) (Integer >= 2).

DT(i)      Time increment (Real > 0.0).

NO(i)      Skip factor for output (every NO(i)th step will be saved for
           output.) (Integer > 0).

## Remarks

1. TSTEP cards must be selected in the Case Control Deck (TSTEP = SID) in
   order to be used by NASTRAN.

2. In aeroelastic response problems, this card is required only when TLOAD is
   requested, that is, when Fourier methods are selected.
