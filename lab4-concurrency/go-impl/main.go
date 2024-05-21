package main

import (
	"log"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"
)

const dishCount = 3
const philosopherCount = 5
const timeout = 1000 * time.Millisecond

var fmt = log.New(os.Stdout, "", 0)

var dinner sync.WaitGroup

type TryMutex struct {
	mu    sync.Mutex
	lockC chan struct{}
}

func NewTryMutex() *TryMutex {
	tm := &TryMutex{
		lockC: make(chan struct{}, 1),
	}
	tm.lockC <- struct{}{}
	return tm
}

func (tm *TryMutex) Lock() {
	tm.mu.Lock()
	<-tm.lockC
	tm.mu.Unlock()
}

func (tm *TryMutex) Unlock() {
	select {
	case tm.lockC <- struct{}{}:
	default:
		panic("unlock of unlocked mutex")
	}
}

func (tm *TryMutex) TryLock(timeout time.Duration) bool {
	tm.mu.Lock()
	defer tm.mu.Unlock()
	select {
	case <-tm.lockC:
		return true
	case <-time.After(timeout):
		return false
	}
}

func philosopher(phName string, leftHand *sync.Mutex, rightHand *TryMutex) {
	rSleep := func(t time.Duration) {
		time.Sleep(t)
	}
	h := 0
	for h < dishCount {
		fmt.Println("Philosopher #"+phName, "is", "thinking")
		rSleep(time.Duration((rand.Intn(10)+1)*100) * time.Millisecond)

		fmt.Println("Philosopher #"+phName, "is", "hungry")
		leftHand.Lock()

		// Go standard library doesn't provide timed lock for mutex
		// so either we call the built-in TryLock from sync.Mutex several times in a loop
		// or we implement our own TryLock method with Internet's help

		// Go's channels are very close to the Rust's conditional variables from std::sync

		// I decided that Rust's way solution is more elegant
		if rightHand.TryLock(timeout) {
			fmt.Println("Philosopher #"+phName, "is", "eating")
			rSleep(time.Duration((rand.Intn(10)+1)*100) * time.Millisecond)
			h++
			rightHand.Unlock()
		} else {
			fmt.Println("Philosopher #"+phName, "forgot why they took a fork and put it back")
		}
		leftHand.Unlock()
	}
	dinner.Done()
	fmt.Println("Philosopher #"+phName, "is", "leaving")
}

func main() {
	dinner.Add(philosopherCount)
	firstFork := &sync.Mutex{}
	forkLeft := firstFork
	for i := 1; i < philosopherCount; i++ {
		forkRight := NewTryMutex()
		go philosopher(strconv.Itoa(i), forkLeft, forkRight)
		forkLeft = &forkRight.mu
	}
	go philosopher(strconv.Itoa(0), forkLeft, NewTryMutex())
	dinner.Wait()
}
