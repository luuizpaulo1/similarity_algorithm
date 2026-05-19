GHC = ghc
FLAGS = -O2 -Wall

TARGET = simil_prog

all: $(TARGET)

$(TARGET): Main.hs
	$(GHC) $(FLAGS) Main.hs -o $(TARGET)

clean:
	rm -f *.o *.hi $(TARGET)

run: all
	./$(TARGET) res.txt sep.txt c1.txt c2.txt
