#include <fstream>
#include <vector>

//! [binary_include]
#include <cereal/archives/binary.hpp> // includes the cereal::BinaryOutputArchive
//! [binary_include]
//! [vector_include]
#include <cereal/types/vector.hpp>    // includes cerealisation support for std::vector
//! [vector_include]

#include <bio/test/tmp_filename.hpp>

// Written for std::vector, other types also work.
void store(std::vector<int16_t> const & data, bio::test::tmp_filename & tmp_file)
{
    std::ofstream os(tmp_file.get_path(), std::ios::binary); // Where output should be stored.
    cereal::BinaryOutputArchive archive(os);                 // Create an ouput archive from the output stream.
    archive(data);                                           // Store data.
}

int main()
{
    // The following example is for an std::vector but any bio data structure that is documented as serialisable
    // could be used, e.g. bio::fm_index.
    bio::test::tmp_filename tmp_file{"data.out"}; // This is a temporary file, use any other filename.

    std::vector<int16_t> vec{1,2,3,4};
    store(vec, tmp_file);                            // Calls store on a std::vector.

    return 0;
}
