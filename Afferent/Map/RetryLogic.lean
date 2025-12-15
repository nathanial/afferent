/-
  Retry Logic with Exponential Backoff

  This module defines pure functions for retry state transitions.
  The formal proofs (safety, liveness) are available in RetryLogicProofs.lean
  when Mathlib is available.

  Safety: If retries are exhausted, no more retries will occur
  Liveness: If retries remain and enough time has passed, a retry will occur

  Ported from heavenly-host to Afferent
-/

namespace Afferent.Map.RetryLogic

/-- Configuration for retry behavior -/
structure RetryConfig where
  maxRetries : Nat      -- Maximum retry attempts (e.g., 3)
  baseDelay : Nat       -- Base delay in time units (e.g., 60 frames = 1 second at 60fps)
  deriving Repr, Inhabited, DecidableEq

/-- Per-tile retry state -/
structure RetryState where
  retryCount : Nat      -- Number of retry attempts so far
  lastFailTime : Nat    -- Abstract time of last failure
  errorMessage : String := ""  -- Most recent error message (for debugging)
  deriving Repr, Inhabited, DecidableEq

namespace RetryState

/-- Calculate exponential backoff delay: baseDelay * 2^retryCount -/
def backoffDelay (config : RetryConfig) (s : RetryState) : Nat :=
  config.baseDelay * (2 ^ s.retryCount)

/-- Calculate next retry time: lastFailTime + backoffDelay -/
def nextRetryTime (config : RetryConfig) (s : RetryState) : Nat :=
  s.lastFailTime + backoffDelay config s

/-- Check if retries are exhausted -/
def isExhausted (config : RetryConfig) (s : RetryState) : Bool :=
  s.retryCount >= config.maxRetries

/-- Decide whether a retry should occur at time tau -/
def shouldRetry (config : RetryConfig) (s : RetryState) (tau : Nat) : Bool :=
  !isExhausted config s && tau >= nextRetryTime config s

/-- Create initial failure state at time tau -/
def initialFailure (tau : Nat) (msg : String := "") : RetryState :=
  { retryCount := 0, lastFailTime := tau, errorMessage := msg }

/-- Record a retry failure, incrementing the count -/
def recordRetryFailure (s : RetryState) (tau : Nat) (msg : String := "") : RetryState :=
  { retryCount := s.retryCount + 1, lastFailTime := tau, errorMessage := msg }

end RetryState

/-! ## Helper: iterate recording failures n times -/
def iterateFailures (tau : Nat) : Nat → RetryState → RetryState
  | 0, s => s
  | n + 1, s => iterateFailures tau n (s.recordRetryFailure tau)

end Afferent.Map.RetryLogic
