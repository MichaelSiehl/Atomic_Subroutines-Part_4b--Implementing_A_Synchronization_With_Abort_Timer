# Atomic_Subroutines-Part_4b--Implementing_A_Synchronization_With_Abort_Timer
Fortran 2008 coarray programming with unordered execution segments (user-defined ordering) and customized synchronization procedures - Atomic Subroutines - Part 4b: How to cope with unreliable data transfers at low-level PGAS programming - allow for safe remote data movement among a number of coarray images. Implementing a customized synchronization procedure with local abort timer.

# Overview
A synchronization process may (partly) fail to complete. The causes of such failures can be due to algorithm, programming, or hardware failures. We have already implemented sufficient synchronization diagnostics and (remote) abort functionality to handle such failures with our customized synchronization procedures: https://github.com/MichaelSiehl/Atomic_Subroutines-Part_4--How_To_Cope_With_Unreliable_Data_Transfers .<br />

Nevertheless, the remote abort functionality alone may not be sufficient (in case it fails) or may be impractical (error-prone) to use on a regular basis. Therefore, this GitHub repository contains a first implementation of a customized synchronization procedure (customized EventWait) that offers a time limit for it's execution. That means, the synchronization process does terminate itself after some time limit is exceeded. While this may lead to party failure of a synchronization process (in case the time limit was choosen to small for the synchronization process to complete as a whole), it is also a simple but efficient way to avoid loss of coarray images due to synchronization failures because of algorithm, programming, or hardware failures on another coarray image. Again, only few lines of Fortran code were required to accomplish that.<br />

# How it works
