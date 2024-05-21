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

var fmt = log.New(os.Stdout, "", 0)

var dinner sync.WaitGroup

func philosopher(phName string, leftHand, rightHand *sync.Mutex) {
	rSleep := func(t time.Duration) {
		time.Sleep(t)
	}
	for h := 0; h < dishCount; h++ {
		fmt.Println("Philosopher #"+phName, "is", "thinking")
		rSleep(time.Duration((rand.Intn(10)+1)*100) * time.Millisecond)
		fmt.Println("Philosopher #"+phName, "is", "hungry")
		leftHand.Lock()
		rightHand.Lock()
		fmt.Println("Philosopher #"+phName, "is", "eating")
		rSleep(time.Duration((rand.Intn(10)+1)*100) * time.Millisecond)
		rightHand.Unlock()
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
		forkRight := &sync.Mutex{}
		go philosopher(strconv.Itoa(i), forkLeft, forkRight)
		forkLeft = forkRight
	}
	go philosopher(strconv.Itoa(0), forkLeft, firstFork)
	dinner.Wait()
}
