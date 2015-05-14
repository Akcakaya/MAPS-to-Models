(*******************  Avian Density Dependence (AvianDD) *************************)
// Density dependence for 2-stage bird models, based on MAPS analysis.
//
// This is the source code for the Delphi programming environment of 
// the density dependence function to be used in a population projection 
// model. This code implements the density dependence functions estimated
// for 9 bird species in the manuscript:
//
// Ryu et al. Developing Population Models with Data from Marked Individuals.
//
// When compiled as a DLL, this code can be used as a "User-defined Density 
// Dependence" function in the PVA programs RAMAS Metapop or RAMAS GIS.
//
// For details and for information on the current status of the manuscript, 
// please contact Resit.Akcakaya@stonybrook.edu
//
// The function is based on population density (calculated in this program as 
// the variable PopDens), which is equal to population size at a given time 
// step (calculated as the variable PopSize), divided by the long-term average 
// of the population size (entered in the program as the parameter CarryingCapacity).  
// Other parameters are listed below.
//
// The function works at two levels:
// 1.  If PopDens>MaxPopDens (or projected to be in the next time step), then 
//     the abundance and the stage matrix is adjusted so that the population 
//     density in the next time step is around MaxPopDens.
// 2.  Otherwise, the stage matrix is adjusted according to the fecundity-density 
//     and/or survival-density functions given in the paper:
//     Fecundity  = F_mean * exp(F_beta_rD * ( (PopDens - MeanDens) / SD_Dens ) )
//     Sjuv = exp(S_intcpt + S_st + S_dens*PopDens)/(1 + exp(S_intcpt + S_st + S_dens*PopDens)) 
//     Sad  = exp(S_intcpt        + S_dens*PopDens)/(1 + exp(S_intcpt        + S_dens*PopDens)) 
//     Fecundity function is used only if F_beta_rD < 0.  
//     Survival functions are used only if S_dens < 0. 
//     The above functions for survival rates are assumed to give apparent survival rates.
//        Thus, survival rates are corrected to account for the difference between apparent
//        and true survival, by multiplying the result of the above functions with a 
//        user-defined parameter (TrueScorr).
//
//
//     Below are the parameter values estimated in this study:
// Species S_intcpt S_st	S_dens	F_mean F_beta_rD MeanDens SD_Dens MaxPopDens
//   NOCA	 0.0256	-1.8453	 0.0524	1.6960	-0.0828	1.5018	1.1340	3.14
//   WEVI	-0.1101	-1.9183	-0.0431	2.6906	-0.1383	1.5814	1.3327	3.72
//   GRCA	-0.3432	-2.4504	 0.1499	4.2826	-0.1217	1.7566	1.7780	4.33
//   COYE	-0.4155	-2.1319	 0.0658	2.0030	-0.0181	1.6429	1.5368	3.74
//   WOTH	-0.4659	-2.5329	-0.0217	3.4297	-0.0063	1.4285	0.9879	2.99
//   HOWA	-0.3300	-2.3111	 0.1476	4.8493	-0.1216	1.4326	1.2881	2.70
//   YBCH	-0.2688	-2.2526	-0.0066	1.8063	0.2578	1.5526	1.4940	4.14
//   CACH	-0.1718	-0.6172	-0.0264	0.7212	-0.2454	1.4285	0.9386	2.25
//   BCCH	-0.3032	-1.1534	 0.0036	1.1754	-0.0279	1.6382	1.6469	3.04



library AvianDD;
{$R+,I+,Q+}
uses
  sysutils,DLL_Intf;
const
  UserParameterCount = 9;
  filename = 'C:\Resit\Current\Bird refugia\MAPS analysis\test-maps.txt' ; // output file for debugging
  debugging = FALSE;
var
  fout:text;

(***************************** PARAMETERS *************************************)
// These parameters are in the "User-defined" tab of the Populations dialog
// under the Model menu of RAMAS Metapop, which is where the user will enter
// the values for each parameter in each population.
// For model desciption, see the manuscript referenced above.
//
// S_intcpt  : intercept of the survival function
// S_st      : stage parameter (for juveniles) of the survival function
// S_dens    : density parameter of the survival function
// F_mean    : mean fecundity
// F_beta_rD : density parameter of the fecundity function
// MeanDens  : average density (average relative population size)
// SD_Dens   : standard deviation of density (relative population size)
// MaxPopDens: maximum value of density (relative population size), above which
//             a ceiling-type density dependence model is used
// TrueScorr : True Survival Correction: multiplier for calculating true survival
//             rate from appearant survival rate: TrueSurv = TrueScorr * AppSurv

// Information about the user parameters entered below include.
//   UserParameterNames (descriptive names with up to 10 characters)
//   UserParameterMinimums (minimum values allowed)
//   UserParameterMaximums (maximum values allowed)
//   UserParameterDefaults (default values used when a new pop is added)
//   UserParameterDecimals (number of digits allowed after decimal point)

const
  UserParameterNames: array[1..UserParameterCount] of PAnsiChar = (
    'S-intcpt', 'Sst', 'S-dens', 'F-mean', 'F-beta_rD', 'MeanDens', 'SDDens', 'max(N/K)', 'TrueScorr');
  UserParameterMinimums: array[1..UserParameterCount] of Double = (
    -10 ,         -10,     -10,     0,      -10,          0,       0.0001,     0, 0);
  UserParameterMaximums: array[1..UserParameterCount] of Double = (
     10 ,          1,       1,      1000,    1,            1000,     1000,    9999, 10);
  UserParameterDefaults: array[1..UserParameterCount] of Double = (
    -0.2 ,         1,      1,      2.0,       1,         1.0 ,    1.0,         1.0, 1.0 );
  UserParameterDecimals: array[1..UserParameterCount] of Cardinal = (
       4,          4,      4,         4,       4,          4,       4,        2, 4);

//---------------------------------------------------------------------------
procedure GetUserParametersInfo(
  var names   : TPCharArrayPtr;
  var minimums: TDoubleArrayPtr;
  var maximums: TDoubleArrayPtr;
  var defaults: TDoubleArrayPtr;
  var decimals: TCardinalArrayPtr;
  var count   : Integer); stdcall;
begin
  names    := @UserParameterNames;
  minimums := @UserParameterMinimums;
  maximums := @UserParameterMaximums;
  defaults := @UserParameterDefaults;
  decimals := @UserParameterDecimals;
  count    := High(UserParameterNames);
end; //GetUserParametersInfo

(**************************************************************************)
// Variables that the DLL needs to remember between calls.
// The DLL is called once per population per time step per replication.
  var
   popcount : Integer;

(************************** INITIALIZATION ****************************)
// These statements are executed at the beginning of a simulation.
// This procedure is called once when the simulation starts, before
// ComputeDensityDependence is called.
procedure InitializeSimulation(
      Replications  : Cardinal;   //number of replications for simulation
      Duration      : Cardinal;   //number of timsteps for simulation
      Populations   : Cardinal;   //number of populations
      Stages        : Cardinal;   //number of stages
  var UserErrorCode : Integer); stdcall;
begin
popcount:=populations;
if debugging then
begin
  assign(fout,filename);
  rewrite(fout);
  writeln(fout, ' T  M11    M12    S_juv  S_ad  Fec  lambda');
end;
end; //InitializeSimulation

(************************** FINALIZATION ***************************)
// These statements are executed at the end of a simulation.
procedure FinalizeSimulation; stdcall;
begin
if debugging then close(fout);
end; //FinalizeSimulation

(********************** procedure ComputeDensityDependence*********************)
// The following is the main procedure in which the DD function is implemented.
// Do not change the types of the variables, or their order in the
// procedure declaration below. These should match the decleration in the file
// Dll_Intf.pas.  Although you can change the names, this is not recommended.
//
// For any other changes to the procedure, make sure you know the RAMAS Metapop
// parameters that correspond to the variables in the following declaration.

//---------------------------------------------------------------------------
procedure ComputeDensityDependence(
//This DLL should NOT change the values of the following parameters:
      ThisReplication   : Cardinal; //current replication of the simulation
      Replications      : Cardinal; //number of replications in this simulation
      Timestep          : Cardinal; //current time step of the simulation
      Duration          : Cardinal; //duration (number of time steps) in this simulation
      PopIndex          : Cardinal; //index for this population
      Stages            : Cardinal; //number of stages in the model

      SexStructure      : SexStructureType; //which sex(es) modeled; see manual

      // The following 3 parameters should be used ONLY if SexStructure=TwoSexes
      FemaleStages      : Cardinal;         //number of female stages
      MatingSystem      : MatingSystemType; //see manual
      AverageMates      : Double;           //females per male (if polygynous) or
                                          //males per female (if polyandrous)
                                          //=1 (if monogamous)

      // stage arrays (vectors of stage-specific parameters)
      AvgWeight         : TSingleArrayPtr;  //average weight of individuals in stage
      Exclude           : TBooleanArrayPtr; //whether stage is excluded from total N
      BasisForDD        : TBooleanArrayPtr; //whether stage is used as a basis for
                                          //dens dep (used only if DDbasis=selectedstages)
      Reproductive      : TDoubleArrayPtr;  //proportion of reproductive individuals

      //DD parameters
      DDaffects         : DDactionType;     //which vital rates affected by DD
      DDbasis           : DDBasisType;      //which abundance is DD based on
      Rmax              : Double;           //maximum growth rate for this population
      CarryingCapacity  : Double;           //K for this population

      //Abundance parameters
      N                 : Integer;          //total abundance of this population
      Nstage            : TLongIntArrayPtr; //abundance in each stage
      //Note: N = sum of Nstage (even if exclude=True for some stages)

      ConstraintsMatrix : TDoubleMatrixPtr; //see manual

      UserParams        : TDoubleArrayPtr;  //user-defined parameters

      //This DLL can change the following parameters
      TMatrix           : TDoubleMatrixPtr; //Transition matrix for this population
      //Incorporates "Relative survival" and "Relative fecundity" parameters

      //Error handling:
  var userDDErrorCode     : Integer;      //code for error in this function (see below)
  var userDDErrorMsg      : TCharArray);  // message string about the error (see below)
      //code=0 : no error
      //code=1 : display the message at the end of simulation
      //code>1 : terminate simulation immediately and display the message
      //code<0 : display the message immediately, continue simulation, set code=0,
      //         and delete the message (thus, 1 message displayed per function call)

  stdcall; // THIS LINE MUST REMAIN

(************************** FUNCTION ********************************)
Function NextTotalN: real;
// This function calculates the APPROXIMATE total abundance in the next time step
    var i,j : integer; rtemp: real;
begin
    rtemp := 0;
    for i := 1 to stages do if (DDBasis=AllStages) or BasisForDD^[i] then
        begin
        for j := 1 to Stages do
                 rtemp := rtemp + TMatrix^[i]^[j] * Nstage^[j];
        end; // i
     NextTotalN := rtemp;
 end;

(************************** VARIABLES ********************************)
var
  i, j, newN : Integer;
  S_intcpt, S_st, S_dens, F_mean, F_beta_rD, MeanDens,
  SD_Dens, MaxPopDens, TrueScorr : real;
  PopSize, PopSize2, PopDens, PopDens2, reduce, reduce2 : real;
  Fec, S_ad, S_juv, lambda : real;

(************************** CONSISTENCY CHECK *************************)
// This function checks the consistency of the input parameters.
Procedure CheckInput;
  begin
  if Stages<>2 then
   begin
   userDDErrorCode := 3;
   userDDErrorMsg  := 'This DD function is designed for a 2-stage model';
   end;
   if sexstructure=TwoSexes then
   begin
   userDDErrorCode := 4;
   userDDErrorMsg  := 'This DD function is not designed for a sex-structured model';
   end;
  end; //CheckInput

begin //Procedure ComputeDensityDependence

(************************** DD FUNCTION ********************************)
// This is the DD function. This procedure modifies the following variables:
//   Tmatrix: the transition (or stage) matrix for the particular population
//            at the particular time step.
//   userDDErrorCode: an integer that informs the program about errors
//            (see above).
//   userDDErrorMsg: a string that is displayed by the program if the error
//            code<>0 (see above).

S_intcpt  := userParams[1];
S_st      := userParams[2];
S_dens    := userParams[3];
F_mean    := userParams[4];
F_beta_rD := userParams[5];
MeanDens  := userParams[6];
SD_Dens   := userParams[7];
MaxPopDens:= userParams[8];
TrueScorr := userParams[9];

// Check consistency of input
if (thisreplication<=1) and (timestep=1) then
  begin
  CheckInput;
  end;

// Some checks need to be done at every time step because the value of some
// parameters may change in time.
if CarryingCapacity<=0 then exit;

S_ad  := Tmatrix^[2]^[2];
S_juv := Tmatrix^[2]^[1];
Fec   := Tmatrix^[1]^[2] / S_ad;
lambda:= S_ad + S_juv * Fec;

if debugging then
begin
writeln(fout, ThisReplication);
writeln(fout, timestep:2, ' ',Tmatrix^[1]^[1]:6:3,' ', Tmatrix^[1]^[2]:6:3,' ',
S_juv:6:3,' ',    S_ad:6:3,' ',  Fec:6:3,' ',lambda:6:3);
end;

if abs((Tmatrix^[1]^[2] / S_ad) - (Tmatrix^[1]^[1] / S_juv)) > 0.001
then
begin
   userDDErrorCode := 5;
   userDDErrorMsg  :=
   'This DD function works only if Fec is the same for all stages (i.e., 1st row = F*S_juv , F*S_ad)';
end;

// Calculate the Population size and density (relative pop size)
PopSize:=0;
case DDBasis of
       AllStages     : PopSize := N;
       SelectedStages: for j:=1 to stages do if BasisForDD^[j] then
                           PopSize := PopSize + Nstage^[j];
       FecWeighted   : begin
                       userDDErrorCode := 2;
                       userDDErrorMsg  :=
                       'This function does not work when DD is based on sum of fecundity*abundances';
                       end;
       end; // case DDBasis

PopSize2 :=   NextTotalN;
PopDens := PopSize / CarryingCapacity; // current time step
PopDens2:= PopSize2 / CarryingCapacity; // next time step

  // Find the reduction in stage matrix elements

{   if (PopDens2>MaxPopDens) then reduce := (MaxPopDens/PopDens2)/lambda
   else if (PopDens>MaxPopDens) then reduce := (MaxPopDens/PopDens)/lambda
   else reduce :=1;  }

   if (PopDens>MaxPopDens) then reduce := (MaxPopDens/PopDens)
   else reduce :=1;
   if (PopDens2>MaxPopDens) then reduce2 := (MaxPopDens/PopDens2)/lambda
   else reduce2 :=1;


       if debugging then
        begin
        end; // debug

  // Reduce the stage matrix

  if (reduce < 1) or (reduce2 < 1) then
    begin // above MaxPopDens: ceiling model

            // reduce N and Nstage
            newN:=0;
            for i := 1 to Stages do
            begin
            Nstage[i] := round(Nstage[i] * reduce);
            newN := newN + Nstage[i];
            end;
            N:=newN;

            // reduce stage matrix
            for i := 1 to Stages do for j := 1 to Stages do
              begin
              Tmatrix^[i]^[j] := reduce2 * Tmatrix^[i]^[j];
              end; // i

             if debugging then
              begin
              S_ad  := Tmatrix^[2]^[2];
              S_juv := Tmatrix^[2]^[1];
              Fec   := Tmatrix^[1]^[2] / S_ad;
              end;

    end // above MaxPopDens: ceiling model

else
    begin // below MaxPopDens: DD functions for Fecundity and Survival rates

    if F_beta_rD < 0 then
        begin
        Fec   := F_mean * exp(F_beta_rD * ( (PopDens - MeanDens) / SD_Dens ) );
        end; // fec DD function
    if S_dens < 0 then
        begin
        S_juv := exp(S_intcpt      + S_st + S_dens*PopDens)/
                 (1 + exp(S_intcpt + S_st + S_dens*PopDens)) ;
        S_ad  := exp(S_intcpt             + S_dens*PopDens)/
                 (1 + exp(S_intcpt        + S_dens*PopDens)) ;
        S_juv := S_juv * TrueScorr;
        S_ad := S_ad * TrueScorr;
        end; // surv DD function

    Tmatrix^[1]^[1] := Fec * S_juv;
    Tmatrix^[1]^[2] := Fec * S_ad;
    Tmatrix^[2]^[1] := S_juv;
    Tmatrix^[2]^[2] := S_ad;

    end;  // below MaxPopDens: DD functions

    if debugging then
        begin
        lambda:=S_ad+S_juv*Fec;
        // writeln(fout,'D= ',PopDens:8:4,' N=',PopSize:5:1,' K=',CarryingCapacity:5:1);
        writeln(fout,'D= ',PopDens:8:4,' N=',PopSize:5:1,' K=',CarryingCapacity:5:1);
        writeln(fout,'D2= ',PopDens2:8:4,' N2=',PopSize2:5:1,' red=',reduce:5:3);
        writeln(fout, timestep:2, ' ',Tmatrix^[1]^[1]:6:3,' ', Tmatrix^[1]^[2]:6:3,' ',
        S_juv:6:3,' ',    S_ad:6:3,' ',  Fec:6:3,' ',lambda:6:3);
        flush(fout);
        end; // debug

end; // Procedure ComputeDensityDependence


exports
  InitializeSimulation,
  FinalizeSimulation,
  ComputeDensityDependence,
  GetUserParametersInfo;

// the following code lets the compiler verify that the function definitions
// match the type definitions
var
  ComputeDensityDependenceFunctionCheck : ComputeDensityDependenceFunctionType;
  GetUserParametersInfoFunctionCheck    : GetUserParametersInfoFunctionType;

begin
  ComputeDensityDependenceFunctionCheck := ComputeDensityDependence;
  if Assigned(ComputeDensityDependenceFunctionCheck) then
    ;

  GetUserParametersInfoFunctionCheck := GetUserParametersInfo;
  if Assigned(GetUserParametersInfoFunctionCheck) then
    ;
end.
