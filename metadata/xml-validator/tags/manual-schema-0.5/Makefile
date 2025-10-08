CXXFLAGS += -std=c++11 -Wall -Weffc++ -pedantic -Werror
LDFLAGS += -lxerces-c -lboost_system -lboost_filesystem
TARGET := validator
DEBUG_FLAGS := -DDEBUG
HEADERS = $(shell find headers -type f -name *.hpp)
SOURCES = $(shell find source -type f -name *.cpp)
OBJECTS = $(patsubst source/%, build/%, $(SOURCES:.cpp=.o))

$(TARGET): $(OBJECTS)
	$(CXX) $(LDFLAGS) $^ -o target/$(TARGET)

build/%.o: source/%.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

cleantarget:
	$(RM) target/$(TARGET)

cleanobj:
	$(RM) build/*.o

clean: cleanobj cleantarget

.PHONY: cleanobj cleantarget clean
