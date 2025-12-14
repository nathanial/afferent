/**
 * Assimp Asset Loader
 * Loads 3D models (FBX, OBJ) via Assimp and converts to Afferent's vertex format.
 * Vertex format: 12 floats per vertex (position[3], normal[3], uv[2], color[4])
 */

#include <assimp/Importer.hpp>
#include <assimp/scene.h>
#include <assimp/postprocess.h>
#include <string>
#include <vector>
#include <map>
#include <cstring>
#include <cstdlib>

extern "C" {
#include "afferent.h"
}

// Recursively collect all meshes from the scene graph
static void collectMeshes(const aiScene* scene, const aiNode* node,
                          std::vector<unsigned int>& meshIndices) {
    for (unsigned int i = 0; i < node->mNumMeshes; i++) {
        meshIndices.push_back(node->mMeshes[i]);
    }
    for (unsigned int i = 0; i < node->mNumChildren; i++) {
        collectMeshes(scene, node->mChildren[i], meshIndices);
    }
}

extern "C" {

AfferentResult afferent_asset_load(
    const char* file_path,
    const char* base_path,
    float** out_vertices,
    uint32_t* out_vertex_count,
    uint32_t** out_indices,
    uint32_t* out_index_count,
    uint32_t** out_submesh_index_offsets,
    uint32_t** out_submesh_index_counts,
    uint32_t** out_submesh_texture_indices,
    uint32_t* out_submesh_count,
    char*** out_texture_paths,
    uint32_t* out_texture_count
) {
    Assimp::Importer importer;

    const aiScene* scene = importer.ReadFile(file_path,
        aiProcess_Triangulate |
        aiProcess_GenSmoothNormals |
        aiProcess_FlipUVs |
        aiProcess_CalcTangentSpace |
        aiProcess_JoinIdenticalVertices |
        aiProcess_SortByPType
    );

    if (!scene || !scene->mRootNode || (scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE)) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Collect all mesh indices from scene graph
    std::vector<unsigned int> meshIndices;
    collectMeshes(scene, scene->mRootNode, meshIndices);

    if (meshIndices.empty()) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Collect unique texture paths from materials
    std::vector<std::string> texturePaths;
    std::map<std::string, uint32_t> textureMap;

    for (unsigned int i = 0; i < scene->mNumMaterials; i++) {
        aiMaterial* mat = scene->mMaterials[i];
        aiString path;
        if (mat->GetTexture(aiTextureType_DIFFUSE, 0, &path) == AI_SUCCESS) {
            std::string pathStr = std::string(base_path) + "/" + path.C_Str();
            if (textureMap.find(pathStr) == textureMap.end()) {
                textureMap[pathStr] = (uint32_t)texturePaths.size();
                texturePaths.push_back(pathStr);
            }
        }
    }

    // Calculate total vertices and indices
    uint32_t totalVertices = 0;
    uint32_t totalIndices = 0;
    for (unsigned int idx : meshIndices) {
        aiMesh* mesh = scene->mMeshes[idx];
        totalVertices += mesh->mNumVertices;
        for (unsigned int f = 0; f < mesh->mNumFaces; f++) {
            totalIndices += mesh->mFaces[f].mNumIndices;
        }
    }

    // Allocate output arrays
    *out_vertices = (float*)malloc(totalVertices * 12 * sizeof(float));
    *out_indices = (uint32_t*)malloc(totalIndices * sizeof(uint32_t));
    *out_submesh_index_offsets = (uint32_t*)malloc(meshIndices.size() * sizeof(uint32_t));
    *out_submesh_index_counts = (uint32_t*)malloc(meshIndices.size() * sizeof(uint32_t));
    *out_submesh_texture_indices = (uint32_t*)malloc(meshIndices.size() * sizeof(uint32_t));

    if (!*out_vertices || !*out_indices || !*out_submesh_index_offsets ||
        !*out_submesh_index_counts || !*out_submesh_texture_indices) {
        free(*out_vertices);
        free(*out_indices);
        free(*out_submesh_index_offsets);
        free(*out_submesh_index_counts);
        free(*out_submesh_texture_indices);
        return AFFERENT_ERROR_BUFFER_FAILED;
    }

    // Fill vertex and index data
    uint32_t vertexOffset = 0;
    uint32_t indexOffset = 0;
    uint32_t submeshIdx = 0;

    for (unsigned int meshIdx : meshIndices) {
        aiMesh* mesh = scene->mMeshes[meshIdx];

        (*out_submesh_index_offsets)[submeshIdx] = indexOffset;

        // Count indices for this submesh
        uint32_t meshIndexCount = 0;
        for (unsigned int f = 0; f < mesh->mNumFaces; f++) {
            meshIndexCount += mesh->mFaces[f].mNumIndices;
        }
        (*out_submesh_index_counts)[submeshIdx] = meshIndexCount;

        // Get texture index for this mesh's material
        uint32_t texIdx = UINT32_MAX;
        if (mesh->mMaterialIndex < scene->mNumMaterials) {
            aiMaterial* mat = scene->mMaterials[mesh->mMaterialIndex];
            aiString path;
            if (mat->GetTexture(aiTextureType_DIFFUSE, 0, &path) == AI_SUCCESS) {
                std::string pathStr = std::string(base_path) + "/" + path.C_Str();
                auto it = textureMap.find(pathStr);
                if (it != textureMap.end()) {
                    texIdx = it->second;
                }
            }
        }
        (*out_submesh_texture_indices)[submeshIdx] = texIdx;

        // Copy vertices (12 floats each: pos[3], normal[3], uv[2], color[4])
        for (unsigned int v = 0; v < mesh->mNumVertices; v++) {
            float* vtx = *out_vertices + (vertexOffset + v) * 12;

            // Position
            vtx[0] = mesh->mVertices[v].x;
            vtx[1] = mesh->mVertices[v].y;
            vtx[2] = mesh->mVertices[v].z;

            // Normal
            if (mesh->HasNormals()) {
                vtx[3] = mesh->mNormals[v].x;
                vtx[4] = mesh->mNormals[v].y;
                vtx[5] = mesh->mNormals[v].z;
            } else {
                vtx[3] = 0.0f;
                vtx[4] = 1.0f;
                vtx[5] = 0.0f;
            }

            // UV coordinates
            if (mesh->HasTextureCoords(0)) {
                vtx[6] = mesh->mTextureCoords[0][v].x;
                vtx[7] = mesh->mTextureCoords[0][v].y;
            } else {
                vtx[6] = 0.0f;
                vtx[7] = 0.0f;
            }

            // Color (default to white if no vertex colors)
            if (mesh->HasVertexColors(0)) {
                vtx[8] = mesh->mColors[0][v].r;
                vtx[9] = mesh->mColors[0][v].g;
                vtx[10] = mesh->mColors[0][v].b;
                vtx[11] = mesh->mColors[0][v].a;
            } else {
                vtx[8] = 1.0f;
                vtx[9] = 1.0f;
                vtx[10] = 1.0f;
                vtx[11] = 1.0f;
            }
        }

        // Copy indices (adjusted for vertex offset)
        uint32_t localIndexOffset = 0;
        for (unsigned int f = 0; f < mesh->mNumFaces; f++) {
            aiFace& face = mesh->mFaces[f];
            for (unsigned int i = 0; i < face.mNumIndices; i++) {
                (*out_indices)[indexOffset + localIndexOffset + i] =
                    vertexOffset + face.mIndices[i];
            }
            localIndexOffset += face.mNumIndices;
        }

        vertexOffset += mesh->mNumVertices;
        indexOffset += meshIndexCount;
        submeshIdx++;
    }

    *out_vertex_count = totalVertices;
    *out_index_count = totalIndices;
    *out_submesh_count = (uint32_t)meshIndices.size();

    // Copy texture paths
    *out_texture_count = (uint32_t)texturePaths.size();
    if (texturePaths.empty()) {
        *out_texture_paths = nullptr;
    } else {
        *out_texture_paths = (char**)malloc(texturePaths.size() * sizeof(char*));
        for (size_t i = 0; i < texturePaths.size(); i++) {
            (*out_texture_paths)[i] = strdup(texturePaths[i].c_str());
        }
    }

    return AFFERENT_OK;
}

void afferent_asset_free_vertices(float* vertices) {
    free(vertices);
}

void afferent_asset_free_indices(uint32_t* indices) {
    free(indices);
}

void afferent_asset_free_submeshes(
    uint32_t* index_offsets,
    uint32_t* index_counts,
    uint32_t* texture_indices
) {
    free(index_offsets);
    free(index_counts);
    free(texture_indices);
}

void afferent_asset_free_texture_paths(char** paths, uint32_t count) {
    if (paths) {
        for (uint32_t i = 0; i < count; i++) {
            free(paths[i]);
        }
        free(paths);
    }
}

} // extern "C"
