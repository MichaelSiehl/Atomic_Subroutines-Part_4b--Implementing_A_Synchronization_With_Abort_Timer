# Atomic_Subroutines-Part_4b--Implementing_A_Synchronization_With_Abort_Timer
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 4b: How to cope with unreliable data transfers at low-level PGAS programming - allow for safe remote data movement among a number of coarray images. Implementing a customized synchronization procedure with local abort timer.

# Overview
A synchronization process may (partly) fail to complete. The causes of such failures can be due to algorithm, programming, or hardware failures. We have already implemented sufficient synchronization diagnostics and (remote) abort functionality to handle such failures with our customized synchronization procedures: https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers .<br />

Nevertheless, the remote abort functionality alone may not be sufficient (in case it fails) or may be impractical (error-prone) to use on a regular basis. Therefore, this GitHub repository contains a first implementation of a customized synchronization procedure (customized EventWait) that offers a time limit for it's execution. That means, the synchronization process does terminate itself after some time limit is exceeded. While this may lead to party failure of a synchronization process (in case the time limit was choosen to small for the synchronization process to complete as a whole), it is also a simple but efficient way to avoid loss of coarray images due to synchronization failures because of algorithm, programming, or hardware failures on another coarray image. Again, only few lines of Fortran code were required to accomplish that.<br />

# How it works
The codes in the src folder should be compiled and run with OpenCoarray/GFortran or with ifort (since version 18 update 1) unsing 6 coarray images.<br />

The Main.f90 contains a simple test case for testing the newly added abort timer functionality of the customized EventWait procedure: The test case does execute a customized EventWait on coarray image 1 and calls to customized EventPost on coarray images 4-6 resp. The customized EventWait on coarray image 1 does try to synchronize with calls to customized EventPost on coarray images 3-6. But, because we do not execute a customized EventPost on coarray image 3, this call of the customized EventWait will never complete successfully as a whole and will complete only partly without coarray image 3. The (optional) reaTimeLimitInSeconds argument of the customized EventWait procedure is set to 1 second. Thus, the customized EventWait will terminate it's execution if the synchronization process is still running after 1 second.<br />

Calling the customized EventWait on coarray image 1 with abort timer enabled after 1 second cpu time (from Main.f90):
```fortran
call OOOPimscEventWaitScalar_intImageActivityFlag99_CA (OOOPimscImageStatus_CA_1, intImageActivityFlag, &
       intNumberOfRemoteImages, intA_RemoteImageNumbers, &
       intA_RemoteImageAndItsAdditionalAtomicValue = intA_RemoteImageAndItsAdditionalAtomicValue, &
       intCheckRemoteAbortOfSynchronization = intCheckRemoteAbortOfSynchronization, &
       logRemoteAbortOfSynchronization = logRemoteAbortOfSynchronization, &
       intRemoteImageThatDidTheAbort = intRemoteImageThatDidTheAbort, &
       intNumberOfSuccessfulRemoteSynchronizations = intNumberOfSuccessfulRemoteSynchronizations, &
       intA_TheSuccessfulImageNumbers = intA_TheSuccessfulImageNumbers, &
       intNumberOfFailedRemoteSynchronizations = intNumberOfFailedRemoteSynchronizations, &
       intA_TheFailedImageNumbers = intA_TheFailedImageNumbers, &
       logActivateCircularSynchronization = .true., &
       reaTimeLimitInSeconds = 1.0) ! activates the local abort timer for the spin-wait loop
```

See the following output from running the test case in Main.f90:
```fortran
invovled remote images:                        0           4           5           6
and the additional atomic values:              0           8          10          12
abort of synchronization (TRUE/FALSE): T
coarray image that did the abort:           1
number of successful remote synchronizations:           3
the successful image numbers:           6           4           5           0
number of failed remote synchronizations:           1
the failed image numbers:           3           0           0           0
```
Here, the 'remote abort of synchronization status' is TRUE and the 'coarray image that did the abort' is image 1 (which is also the image that did execute the customized EventWait). Thus, the synchronization abort was initiated locally on coarray image 1 through the abort timer of the customized EventWait procedure after 1 second (cpu time).<br />
