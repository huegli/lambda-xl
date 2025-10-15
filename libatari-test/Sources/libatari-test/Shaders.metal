#include <metal_stdlib>
using namespace metal;

// Vertex shader output / Fragment shader input
struct RasterizerData {
    float4 position [[position]];
    float2 texCoord;
};

// Vertex shader for full-screen quad
vertex RasterizerData vertexShader(uint vertexID [[vertex_id]]) {
    // Full-screen quad vertices (2 triangles)
    const float2 positions[6] = {
        float2(-1.0, -1.0),  // Bottom-left
        float2( 1.0, -1.0),  // Bottom-right
        float2(-1.0,  1.0),  // Top-left
        float2(-1.0,  1.0),  // Top-left
        float2( 1.0, -1.0),  // Bottom-right
        float2( 1.0,  1.0)   // Top-right
    };
    
    const float2 texCoords[6] = {
        float2(0.0, 1.0),
        float2(1.0, 1.0),
        float2(0.0, 0.0),
        float2(0.0, 0.0),
        float2(1.0, 1.0),
        float2(1.0, 0.0)
    };
    
    RasterizerData out;
    out.position = float4(positions[vertexID], 0.0, 1.0);
    out.texCoord = texCoords[vertexID];
    return out;
}

// Fragment shader that converts Atari palette index to RGB
fragment float4 fragmentShader(RasterizerData in [[stage_in]],
                               texture2d<float> atariTexture [[texture(0)]],
                               constant float4 *colorLookup [[buffer(0)]]) {
    constexpr sampler textureSampler(mag_filter::nearest, min_filter::nearest);
    
    // Sample the palette index from the texture (stored in red channel)
    float paletteIndex = atariTexture.sample(textureSampler, in.texCoord).r;
    
    // Convert to integer index (0-255)
    int index = int(paletteIndex * 255.0);
    
    // Look up the actual RGB color from the palette
    return colorLookup[index];
}
